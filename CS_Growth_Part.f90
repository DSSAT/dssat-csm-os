!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4707 - 5046 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_Part calculates assimilation partitioning, growth of storage roots, leaves, stems and Plant. sticks.
!***************************************************************************************************************************
    
    SUBROUTINE CS_Growth_Part ( &
        BRSTAGE     , ISWNIT      , NFP         &
        )
    
        USE ModuleDefs
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWNIT      
        REAL    BRSTAGE     , NFP         

        REAL    CSYVAL      , TFAC4                                                                       ! Real function call

        !-----------------------------------------------------------------------
        !           Partitioning of C to above ground and roots (minimum) 
        !-----------------------------------------------------------------------
        
        !LPM 30MAY2015 Delete PTF to consider a spill-over model
        
        !PTF = PTFMN+(PTFMX-PTFMN)*DSTAGE                                                                               !EQN 280   
        ! Partition adjustment for stress effects
        !PTF = AMIN1(PTFMX,PTF-PTFA*(1.0-AMIN1(WFG,NFG)))                                                               !EQN 281
        !CARBOR = AMAX1(0.0,(CARBOBEG+CARBOADJ))*(1.0-PTF)                                                              !EQN 282
        !CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ)) - CARBOR                                                               !EQN 283
        
        CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ))                                                                         !EQN 283
        
        ! Stem fraction or ratio to leaf whilst leaf still growing
        ! (If a constant STFR is entered,swfrx is set=stfr earlier)
        ! Increases linearly between specified limits
        !SWFR = CSYVAL (LNUM,SWFRNL,SWFRN,SWFRXL,SWFRX)                                                                !EQN 296 !LPM 05JUN2015 SWFR is not used 

        ! Plant. stick fraction                                  !LPM 20MAY2015 Deleted, instead it is used NODEWTGB(0)  
        !GROCRFR = 0.0
        !! Increases linearly from start to end of growth cycle
        !GROCRFR = CRFR * DSTAGE                                                                                        !EQN 386

        !-----------------------------------------------------------------------
        !           Number determination of storage root 
        !-----------------------------------------------------------------------

        !GROSR = 0.0 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used  !LPM 05JUN2015 DUSRI is not used
        !IF(CUMDU+DU.LT.DUSRI)THEN 
        !    SRDAYFR = 0.0                                                                                              !EQN 290a
        !ELSEIF(CUMDU.LT.DUSRI.AND.CUMDU+DU.GE.DUSRI)THEN
        !    SRDAYFR = (DUSRI-CUMDU)/DU                                                                                 !EQN 290b
        !ELSEIF(CUMDU.GT.DUSRI)THEN
        !    SRDAYFR = 1.0                                                                                              !EQN 290c
        !ENDIF
        !GROSR = SRFR*CARBOT*SRDAYFR                                                                                    !EQN 289
            
        !IF(CUMDU.GE.DUSRI.AND.SRNOPD.LE.0.0) THEN                                  !LPM 05JUN2015 SRNOPD Defined when SRWT > 0 See CS_Growth_Distribute.f90
        !    SRNOPD = INT(SRNOW*((LFWT+STWT+CRWT+RSWT)))                                                                !EQN 291
        !ENDIF
                     
        !-----------------------------------------------------------------------
        !           Specific leaf area
        !-----------------------------------------------------------------------
        !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA
        
        !IF (LAWTR.GT.0.0.AND.LAWTS.GT.0.0.AND.LAWTS.GT.TMEAN) THEN
        !    TFLAW = 1.0+LAWTR*(TMEAN-LAWTS)                                                                            !EQN 305
        !ELSE
        !    TFLAW = 1.0
        !ENDIF
        !IF (LAWWR.GT.0.0.AND.WFG.LT.1.0) THEN
        !    WFLAW = 1.0+LAWWR*(WFG-1.0)                                                                                !EQN 306
        !ELSE
        !    WFLAW = 1.0
        !ENDIF
        !
        !! Position effect on standard SLA
        !IF (LNUMSG.GT.0) THEN
        !    LAWL(1) = AMAX1(LAWS*LAWFF,LAWS+(LAWS*LAWCF)*(LNUMSG-1))                                                  !EQN 307
        !    ! Temperature and water stress effects on SLA at position
        !    LAWL(1) = AMAX1(LAWL(1)*LAWMNFR,LAWL(1)*TFLAW*WFLAW)                                                      !EQN 308
        !ELSE  
        !    LAWL(1) = LAWS
        !ENDIF 
        
        LAWL(1) = LAWS
        !-----------------------------------------------------------------------
        !           Leaf growth
        !-----------------------------------------------------------------------

        
        GROLF = 0.0
        GROLFADJ = 0.0
        GROLFP = 0.0
        GROLSRS = 0.0
        GROLS = 0.0
        GROLSA = 0.0
        GROLSP = 0.0
        GROLSRT = 0.0
        GROLSSEN = 0.0
        GROLSRTN = 0.0
        GROLSSD = 0.0
        !LAGEG = 0.0   !LPM 28MAR15 This variable is not used
        !PLAGS2 = 0.0  !LPM 23MAR15 non necessary PLAGSB2 considers all the branches and shoots
        SHLAG2 = 0.0
        SHLAG2B = 0.0  !LPM 23MAR15 add new variable
        SHLAGB2 = 0.0
        SHLAGB3 = 0.0
        SHLAGB4 = 0.0
            
        !! BRANCH NUMBER            
        !! Old method (1 fork number throughout)
        !! BRNUMST = AMAX1(1.0,BRNUMFX**(INT(brstage)-1))
        !! New method (fork number specified for each forking point)
        !! First calculate new BRSTAGE as temporary variable
        !! (LAH Check whether can move brstage calc up here! 
        !! (If do this, brstage in brfx below must be reduced by 1))
        !IF (MEDEV.EQ.'LNUM') THEN 
        !    IF (PDL(INT(BRSTAGE)).GT.0.0) THEN                                                          ! MSTG = KEYPSNUM
        !        TVR1 = FLOAT(INT(BRSTAGE)) + (LNUM-LNUMTOSTG(INT(BRSTAGE)))/PDL(INT(BRSTAGE))           ! EQN 004
        !    ELSE
        !        TVR1 = FLOAT(INT(BRSTAGE))
        !    ENDIF
        !ELSE
        !    IF (PD(INT(BRSTAGE)).GT.0.0) THEN                                                          ! MSTG = KEYPSNUM
        !        TVR1 = FLOAT(INT(BRSTAGE)) + (CUMDU-PSTART(INT(BRSTAGE)))/PD(INT(BRSTAGE))              ! EQN 004
        !    ELSE
        !        TVR1 = FLOAT(INT(BRSTAGE))
        !    ENDIF        
        !ENDIF    
        !IF (INT(TVR1).GT.INT(BRSTAGEPREV)) THEN
        !    IF (BRSTAGE.EQ.0.0) THEN
        !        BRNUMST = 1                                                                         ! BRNUMST          ! Branch number/shoot (>forking) # (Actually the total number of apices)
        !    ELSEIF (BRSTAGE.GT.0.0) THEN
        !        BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))                                                ! BRFX(PSX)        ! EQN 005 ! # of branches at each fork # (This is where new branch is initiated)
        !    ENDIF
        !ENDIF 
        
        

        ! Potential leaf size for next growing leaf - main shoot 
        IF (DAE.GT.0.0) THEN
            LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM                                                                           !EQN 332
            IF (ABS(LNUMNEED).LE.1.0E-6) LNUMNEED = 0.0
            !LPM 25/02/2015 the next lines are commented out to change the strategy to estimate the potential leaf area
        
            !IF (LNUMSG+1.LE.INT(LAXNO)) THEN
            !    LAPOTX(LNUMSG+1) = AMIN1(LAXS, LA1S + LNUMSG*((LAXS-LA1S)/(LAXNO-1)))                                      !EQN 319a
            !ELSEIF (LNUMSG+1.GT.INT(LAXNO).AND.LNUMSG+1.LE.INT(LAXN2)) THEN
            !    LAPOTX(LNUMSG+1) = LAXS                                                                                    !EQN 319b
            !ELSE
            !    LAPOTX(LNUMSG+1) = AMAX1(LAFS, LAXS - ((LNUMSG+1)-LAXN2)*((LAXS-LAFS)/(LAFND-LAXN2)))                      !EQN 319c
            !ENDIF
         
            !LPM 28/02/2015 b_slope_lsize=Slope to define the maximum leaf size according to the mean temperature (it should be from the last 10 days)
            !b_slope_lsize = MAX(0.0,0.0375-(0.0071*((TRDV1(3)-TRDV1(2))-TT20)))       ! LPM 28FEB15
        
                
            IF (DAWWP.LT.900) THEN
                LAPOTX(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1)) =  LAXS*((DAWWP*1E-3)+0.10)                  ! LPM 07MAR15 
            ELSE
                IF (DAWWP-TT.LT.900) DALSMAX = DAE                                 ! LPM 28FEB15 to define the day with the maximum leaf size
                !LAPOTX(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1)) = LAXS/((1+(4.154582E-2*(DAE-DALSMAX))))
                !LPM 12JUL2015 test with thermal time with optimum of 20 C
                !LPM 24APR2016 Use of DALS (considering water stress) instead of TTCUMLS
                LAPOTX(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1)) = LAXS/((1+(5.665259E-3*(DALS))))
            ENDIF
                ! LAH Sept 2012 Eliminate fork # effect on leaf size 
            ! Adjust for fork#/shoot
            !IF (BRNUMST.GE.1)LAPOTX(LNUMSG+1)=LAPOTX(LNUMSG+1)/BRNUMST
            ! Keep track of 'forking';reduce potential>forking
            !IF (LNUMSG.GT.1.AND.BRNUMPT.GT.BRNUMSTPREV) THEN
            !  LAPOTX(LNUMSG+1) = LAPOTX(LNUMSG+1)/LAFF
            !  LNUMFORK = LNUMSG
            !ENDIF 
            
            ! Leaf area increase:growing leaves on 1 axis,main shoot
            !SHLAG2(1) = 0.0                                                                             !LPM 23MAR15 Move to the do loop
            !DO L = MAX(1,LNUMSG-(INT((LLIFG/PHINTS)+1))),LNUMSG+1                                       ! MF Why + 1? See LPM p. 63.    !EQN 320
            !    ! Basic leaf growth calculated on thermal time base. 
            !    ! Basic response (cm2/d) same as for development. 
            !    TTNEED = AMAX1(0.0,LLIFG-LAGETT(L))                                                                        !EQN 321
            !    LATLPREV(L) = LATL(L)
            !    LATLPOT(L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)                                                   !EQN 322 
            !    IF (LATLPOT(L).LT.0.0) LATLPOT(L) = 0.0
            !    IF (LATLPOT(L).GT.LAPOTX(L)) LATLPOT(L) = LAPOTX(L)
            !    LATL(l) = LATL(L) + (LATLPOT(L)-LATLPREV(L))                                                               !EQN 323
            !    LATL2(l) = LATL2(L) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                         !EQN 324 
            !    SHLAG2(1) = SHLAG2(1) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                       !EQN 325
            !! The 2 at the end of the names indicates that 2 groups 
            !! of stresses have been taken into account
            !! Stress factors for individual leaves
            !        WFLF(L) = AMIN1(1.0,WFLF(L)+WFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                                        !EQN 326
            !        NFLF(L) = AMIN1(1.0,NFLF(L)+NFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                                        !EQN 327
            !        NFLFP(L) = AMIN1(1.0,NFLFP(L)+NFP*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                                      !EQN 328
            !        TFGLF(L) = AMIN1(1.0,TFGLF(L)+TFG*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                                      !EQN 329
            !        TFDLF(L) = AMIN1(1.0,TFDLF(L)+TFD*(LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                                      !EQN 330
            !        ! New LEAF
            !        IF (L.EQ.LNUMSG.AND.LNUMG.GT.LNUMNEED) THEN                                             ! This is where new leaf is initiated
            !            LAGL(L+1) = LAPOTX(L+1) * (TTLFLIFE*EMRGFR) * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG)      ! LAGL(LNUMX)         ! Leaf area growth,shoot,lf pos  cm2/l   !EQN 331       
            !            LATL(L+1) = LATL(L+1) + LAGL(L+1)                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   !EQN 333   
            !            LATL2(L+1) = LATL2(L+1) + LAGL(L+1) * AMIN1(WFG,NFG)*TFG                            ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l   !EQN 334
            !            SHLAG2(1) = SHLAG2(1) + LAGL(L+1) * AMIN1(WFG,NFG)*TFG                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2     !EQN 335
            !            LBIRTHDAP(L+1) = DAP                                                                ! LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #  
            !            ! Stress factors for individual leaves                       
            !            WFLF(L+1) = AMIN1(1.0,WFLF(L+1)+WFG*LATL(L+1)/LAPOTX(L+1))                                             !EQN 336
            !            NFLF(L+1) = AMIN1(1.0,NFLF(L+1)+NFG*LATL(L+1)/LAPOTX(L+1))                                             !EQN 337
            !            NFLFP(L+1) = AMIN1(1.0,NFLFP(L+1)+NFP*LATL(L+1)/LAPOTX(L+1))                                           !EQN 338
            !            TFGLF(L+1) = AMIN1(1.0,TFGLF(L+1)+TFG*LATL(L+1)/LAPOTX(L+1))                                           !EQN 339
            !            TFDLF(L+1) = AMIN1(1.0,TFDLF(L+1)+TFD*LATL(L+1)/LAPOTX(L+1))                                           !EQN 340
            !        ENDIF
        
                                                                                                              
            DO BR = 0, BRSTAGE                                                                                        !LPM 23MAR15 To consider cohorts
               SHLAG2B(BR) = 0.0  
                DO LF = 1, LNUMSIMSTG(BR)+1
                    IF (LAGETT(BR,LF).LE.LLIFGTT) THEN
                ! Basic leaf growth calculated on chronological time base. 
                ! Basic response (cm2/day) considering a maximum growing duration of 10 days 
                        LATLPREV(BR,LF) = LATL(BR,LF)
                        !LATLPOT(L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)                                                   !EQN 322 !LPM 24APR2016 To estimate daily leaf area increase instead of total
                        LAPOTX2(BR,LF) = LAPOTX(BR,LF)*TFLFLIFE
                        LAGL(BR,LF)=LAPOTX2(BR,LF)*(TTLFLIFE/LLIFGTT)
                        IF (LAGL(BR,LF) < 0.0) THEN
                            LAGL(BR,LF) = 0.0
                        ENDIF
                        IF (LAGL(BR,LF) > (LAPOTX2(BR,LF)-LATL3(BR,LF))) THEN                                           !DA IF there is something left to grow
                            LAGL(BR,LF) = LAPOTX2(BR,LF)-LATL3(BR,LF)
                        ENDIF
                        !LATL(BR,LF) = LATL(BR,LF) + (LATLPOT(BR,LF)-LATLPREV(BR,LF))                                                !EQN 323 !LPM 24APR2016 To estimate daily leaf area increase instead of total
                        LATL(BR,LF) = LATL(BR,LF) + LAGL(BR,LF)                              !Leaf area = Leaf area + leaf growth    !EQN 323
                        LATL(BR,LF) = AMIN1(LATL(BR,LF), LAPOTX(BR,LF))                                                            ! DA 28OCT2016 Limiting LATL to not get over the maximum potential
                        !LATL2(l) = LATL2(L) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                         !EQN 324 LPM 21MAR15 TFG is changed by Tflflife to be able to change the Tb
                        !SHLAG2(1) = SHLAG2(1) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                       !EQN 325
                        !LAGL(BR,LF) = LATLPOT(BR,LF)* AMIN1(WFG,NFG)
                        LATL2(BR,LF) = LATL2(BR,LF) + LAGL(BR,LF)                                  !EQN 324 !LPM 24APR2016 LATL2 with the same value than LATL 
                        LATL2(BR,LF) = AMIN1(LATL2(BR,LF), LAPOTX(BR,LF))                                               !DA 28OCT2016 Limiting LATL2 to not get over the maximum potential
                        SHLAG2B(BR) = SHLAG2B(BR) + LAGL(BR,LF)                                    !EQN 325
                     
                        !LPM 15NOV15 Variables LAGLT and LATL2T created to save the leaf are by cohort (all the plant (all branches and shoots))
                        LAGLT(BR,LF) = LAGL(BR,LF)*BRNUMST(BR) ! To initialize before adding over shoots     
                        LATL2T(BR,LF) = LATL2(BR,LF)*BRNUMST(BR)
                        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                                LAGLT(BR,LF) = LAGLT(BR,LF)+(LAGL(BR,LF)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
                                LATL2T(BR,LF) = LATL2T(BR,LF)+(LATL2(BR,LF)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                            ENDIF
                        ENDDO

              
               
                    ! The 2 at the end of the names indicates that 2 groups 
                    ! of stresses have been taken into account
                    ! Stress factors for individual leaves
                        WFLF(BR,LF) = AMIN1(1.0,WFLF(BR,LF)+WFG*(LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))                                        !EQN 326
                        NFLF(BR,LF) = AMIN1(1.0,NFLF(BR,LF)+NFG*(LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))                                        !EQN 327
                        NFLFP(BR,LF) = AMIN1(1.0,NFLFP(BR,LF)+NFP*(LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))                                      !EQN 328
                        TFGLF(BR,LF) = AMIN1(1.0,TFGLF(BR,LF)+TFG*(LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))                                      !EQN 329
                        TFDLF(BR,LF) = AMIN1(1.0,TFDLF(BR,LF)+TFD*(LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))                                      !EQN 330
                        ! New LEAF
                        IF (LF.EQ.LNUMSIMSTG(BR).AND.LNUMG.GT.LNUMNEED.AND.BR.EQ.BRSTAGE) THEN                                             ! This is where new leaf is initiated
                            !LAGL(BR,L+1) = LAPOTX(BR,L+1) * (TTLFLIFE*EMRGFR) * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG)      ! LAGL(LNUMX)         ! Leaf area growth,shoot,lf pos  cm2/l   !EQN 331  
                            !LAGL(BR,LF+1) = LAPOTX(BR,LF+1) * EMRGFR * ((LNUMG-LNUMNEED)/LNUMG) * AMIN1(WFG,NFG)*Tflflife                                      !LPM 23MAR15 To define proportional growth by day      
                            LAPOTX2(BR,LF+1) = LAPOTX(BR,LF+1)*TFLFLIFE
                            LAGL(BR,LF+1)=LAPOTX2(BR,LF+1)*(TTLFLIFE/LLIFGTT)* EMRGFR * ((LNUMG-LNUMNEED)/LNUMG)   !LPM 02SEP2016 To register the growth of the leaf according LAGL(BR,LF) (see above)
                            IF (LAGL(BR,LF+1).LT.0.0) LAGL(BR,LF+1) = 0.0
                            LATL(BR,LF+1) = LATL(BR,LF+1) + LAGL(BR,LF+1)                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   !EQN 333   
                            LATL2(BR,LF+1) = LATL2(BR,LF+1) + LAGL(BR,LF+1)                              ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l   !EQN 334
                            SHLAG2B(BR) = SHLAG2B(BR) + LAGL(BR,LF+1)                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2     !EQN 335
                            LBIRTHDAP(BR,LF+1) = DAP                                                                ! LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #  
                        
                            LAGLT(BR,LF+1) = LAGL(BR,LF+1)*BRNUMST(BR)
                            LATL2T(BR,LF+1) = LATL2(BR,LF+1)*BRNUMST(BR)
                            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                                IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                                    LAGLT(BR,LF+1) = LAGLT(BR,LF+1)+(LAGL(BR,LF+1)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
                                    LATL2T(BR,LF+1) = LATL2T(BR,LF+1)+(LATL2(BR,LF+1)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                                ENDIF
                            ENDDO
                        
                            ! Stress factors for individual leaves                       
                            WFLF(BR,LF+1) = AMIN1(1.0,WFLF(BR,LF+1)+WFG*LATL(BR,LF+1)/LAPOTX(BR,LF+1))                                             !EQN 336
                            NFLF(BR,LF+1) = AMIN1(1.0,NFLF(BR,LF+1)+NFG*LATL(BR,LF+1)/LAPOTX(BR,LF+1))                                             !EQN 337
                            NFLFP(BR,LF+1) = AMIN1(1.0,NFLFP(BR,LF+1)+NFP*LATL(BR,LF+1)/LAPOTX(BR,LF+1))                                           !EQN 338
                            TFGLF(BR,LF+1) = AMIN1(1.0,TFGLF(BR,LF+1)+TFG*LATL(BR,LF+1)/LAPOTX(BR,LF+1))                                           !EQN 339
                            TFDLF(BR,LF+1) = AMIN1(1.0,TFDLF(BR,LF+1)+TFD*LATL(BR,LF+1)/LAPOTX(BR,LF+1))                                           !EQN 340
                        ENDIF
                    ENDIF
                ENDDO
            SHLAG2(1) = SHLAG2(1)+(SHLAG2B(BR)*BRNUMST(BR))                                                                                            !LPM 23MAR15 To have the leaf area fro one shoot considering all the branches (axis)
            ENDDO
 
            ! Leaf area increase:growing leaves on 1 axis,all shoots
            PLAGSB2 = SHLAG2(1) ! To initialize before adding over shoots                                                  
            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                    PLAGSB2 = PLAGSB2+SHLAG2(1)*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                         !EQN 341
                    SHLAG2(L) = SHLAG2(1)*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                             !EQN 342
                ENDIF
            ENDDO
        

            !! Leaf area increase:growing leaves on all axes,all shoots                                                     !LPM 23MAR15  It is not necessary, previously defined in the loop as SHLAG2
            !PLAGSB2 = PLAGS2*BRNUMST                                                                                       !EQN 343
            !SHLAGB2(1) = SHLAG2(1)*BRNUMST                                                                                 !EQN 344
            !DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main= 1
            !    SHLAGB2(L) =  SHLAG2(L)*BRNUMST
            !ENDDO
            
            ! Potential leaf weight increase.
            IF (LAWL(1).GT.0.0) GROLFP = (PLAGSB2/LAWL(1)) / (1.0-LPEFR)                                                   !EQN 297    
        ENDIF
        
        !LPM 02MAR15 Stem weight increase by cohort: 1 axis,main shoot
        !DO L = 1,LNUMSG+1  
        !    DO I = 0, INT(BRSTAGE)
        !    IF (L.LE.LNUMTOSTG(I-1) THEN
        !        BRNUMST = 1                                                                         
        !    ELSEIF (BRSTAGE.GT.0.0) THEN
        !        BRNUMST = BRNUMST*BRFX(INT(BRSTAGE))                                                
        !    ENDIF        
        
        ! Potential leaf+stem weight increase. !LPM 11APR15 Comment out to test the node development and the potential stem growth 
        !IF (SWFR.GT.0.0.AND.SWFR.LT.1.0) THEN
        !    GROLSP = GROLFP * (1.0 + SWFR/(1.0-SWFR))                                                                  !EQN 295a
        !ELSE
        !    GROLSP = GROLFP                                                                                            !EQN 295b
        !ENDIF
        
        
        !LPM 11APR15  Rate of node weight increase by branch level and cohort  
        NODEWTG = 0.0
        GROSTP = 0.0
        GROCRP = 0.0
        !LPM 01SEP16 putting a conditional DAE > 0.0 to avoid illogical values of NODEWTGB
        IF (DAE > 0.0) THEN
            DO NG = 1, NodeGroupSize          ! DA 13DIC2016 for each group of nodes
                    NODEWTGB(NG) = ((1/(1+(((NG)/3.10036)**5.89925)))*(2.5514108*(((DAE-NDDAE(NG)+1)/171.64793)**-2.2115103)/ & 
                        ((DAE-NDDAE(NG)+1)*(((((DAE-NDDAE(NG)+1)/171.64793)**-2.2115103)+1))**2))*TFD*NODWT)
            ENDDO
        ENDIF

        DO BR = 0, BRSTAGE               ! for each branch   
            DO LF = 1, LNUMSIMSTG(BR)    ! and each node of the branches
                NODEWTG(BR,LF) = NODEWTGB(NodeGroup(BR,LF))
                !IF (BR.EQ.0.AND.LF.EQ.1.AND.DAE.EQ.1.AND.SEEDUSES.GT.0.0) NODEWTG(BR,LF) = SEEDUSES + NODEWTGB(BR) !LPM 22MAR2016 To add the increase of weight from reserves 
                NODEWT(BR,LF) = NODEWT(BR,LF) + NODEWTG(BR,LF)
                GROSTP = GROSTP + (NODEWTG(BR,LF)*BRNUMST(BR)) !LPM08JUN2015 added BRNUMST(BR) to consider the amount of branches by br. level
                STWTP = STWTP + (NODEWT(BR,LF)*BRNUMST(BR))
            ENDDO
        ENDDO
        
        GROCRP = NODEWTGB(0)*SPRL/NODLT   !LPM 02OCT2015 Added to consider the potential increase of the planting stick                
        CRWTP = CRWTP + GROCRP    !LPM 23MAY2015 Added to keep the potential planting stick weight
        GROLSP = GROLFP + GROSTP + GROCRP  !LPM 02OCT2015 Added to consider the potential increase of the planting stick                                                                                    
        
        !-----------------------------------------------------------------------
        !           Root growth                                     
        !-----------------------------------------------------------------------

        !LPM 30MAY2015 Modify RTWTG according to Matthews & Hunt, 1994 (GUMCAS)
        !LPM 05OCT2015 Moved before of leaf, stem and planting stick growth 
        !RTWTG = (CARBOR+SEEDRSAVR)*(1.0-RRESP)                                                                         !EQN 387
        
        IF (CARBOT.GT.0.0.OR.SEEDRSAVR.GT.0.0) THEN  !LPM 23MAR2016  To consider root growth from SEEDRSAVR
            CARBOR = AMAX1(0.0,AMIN1(CARBOT,(GROST+GROLF)*(0.05+0.1*EXP(-0.005*Tfd))))  !LPM 02OCT2015 to avoid negative values of CARBOT
            CARBOT = AMAX1(0.0,CARBOT - CARBOR)
            RTWTG = ((GROST+GROLF)*(0.05+0.1*EXP(-0.0005*Tfd))+SEEDRSAVR)*(1.0-RRESP)                                                                         !EQN 387
            RTRESP = RTWTG*RRESP/(1.0-RRESP)                                                                               !EQN 388
        ENDIF
        
        IF (GROLSP.GT.0.0) THEN
            ! Leaf+stem weight increase from assimilates
            !GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROSR))                                                              !EQN 298 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
            GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT))                                                                     !EQN 298
            ! Leaf+stem weight increase from senescence 
            IF (GROLSA.LT.GROLSP) THEN

                GROLSSEN = AMIN1(GROLSP-GROLSA,SENLFGRS)
            ENDIF
            
            IF (GROLSA+GROLSSEN.LT.GROLSP) THEN
                ! Leaf+stem weight increase from seed reserves
                ! LAH May need to restrict seed use.To use by roots?
                IF (DAE.LE.0.0)THEN !LPM 23MAR2016 Before emergence use the SEEDUSED according with CS_growth_Init.f90
                    GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDUSED)                                                     !EQN 300
                ELSE
                    GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDRSAV)                                                     !EQN 300
                    SEEDRSAV = SEEDRSAV - GROLSSD                                                                          !EQN 288
                ENDIF
               
                
                IF ( LAI.LE.0.0.AND.GROLSSD.LE.0.0.AND.SEEDRSAV.LE.0.0.AND.ESTABLISHED.NE.'Y') THEN
                    CFLFAIL = 'Y'
                    WRITE (Message(1),'(A41)') 'No seed reserves to initiate leaf growth '
                    WRITE (Message(2),'(A33,F8.3,F6.1)') '  Initial seed reserves,seedrate ',seedrsi,sdrate
                    WRITE (Message(3),'(A33,F8.3,F6.1)') '  Reserves %,plant population    ',sdrs,pltpop    !LPM 22MAR2016 Keep value SDRS  
                    CALL WARNING(3,'CSCGR',MESSAGE)
                ENDIF
            ENDIF
            ! Leaf+stem weight increase from plant reserves
            IF (GROLSA+GROLSSD+GROLSSEN.LT.GROLSP) THEN
                GROLSRS =  AMIN1(RSWT*RSUSE,GROLSP-GROLSA-GROLSSD-GROLSSEN)                                            !EQN 301
            ENDIF
            ! Leaf+stem weight increase from roots (after drought)
            GROLSRT = 0.0
            GROLSRTN = 0.0
            IF ((GROLSA+GROLSSD+GROLSRS+GROLSSEN).LT.GROLSP.AND.SHRTD.LT.1.0.AND.RTUFR.GT.0.0.AND.ESTABLISHED.EQ.'Y') THEN
                GROLSRT = AMIN1(RTWT*RTUFR,(GROLSP-GROLSA-GROLSSD-GROLSSEN-GROLSRS))                                   !EQN 302
                IF (ISWNIT.NE.'N') THEN
                    GROLSRTN = GROLSRT * RANC                                                                          !EQN 244
                ELSE
                    GROLSRTN = 0.0
                ENDIF
                WRITE(Message(1),'(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)') &
                    'Roots -> leaves ',' Shoot/root ',shrtd,' Grolsp ',grolsp,' Grols ',grols,' Grolsrt ',grolsrt
                CALL WARNING(1,'CSCGR',MESSAGE)
            ENDIF
            ! Leaf+stem weight increase from all sources
            GROLS = GROLSA + GROLSSEN + GROLSSD + GROLSRS+GROLSRT                                                      !EQN 303
            ! Leaf weight increase from all sources
            IF ((GROLSP).GT.0.0) THEN
                GROLF = GROLS * GROLFP/GROLSP                                                                          !EQN 304
            ELSE  
                GROLF = 0.0
            ENDIF
            ! Check if enough assimilates to maintain SLA within limits
            !AREAPOSSIBLE = GROLF*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))                                                     !EQN 148 !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA 
            AREAPOSSIBLE = GROLF*(1.0-LPEFR)*LAWL(1)
            ! If not enough assim.set assimilate factor
            IF (PLAGSB2.GT.AREAPOSSIBLE.AND.PLAGSB2.GT.0.0)THEN
                AFLF(0,0) = AREAPOSSIBLE/PLAGSB2                                                                         !EQN 149
            ELSE  
                AFLF(0,0) = 1.0
            ENDIF
            IF (CFLAFLF.EQ.'N') AFLF(0,0) = 1.0                                                                            !MF 21AU16 ErrorFix Added second subscript to AFLF.

        !    ! Area and assimilate factors for each leaf
        !        DO BR = 0, BRSTAGE                                                                                        !LPM 23MAR15 To consider cohorts
        !            DO LF = 1, LNUMSIMSTG(BR)
        !            !DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1                                                  !LPM 23MAR15 To consider the new leaf growing duration of 10 
        !                !IF (LNUMSG.LT.LNUMX) THEN
        !                !    LATL3(L)= LATL2(L) * AFLF(0,0)                                                                                   !EQN 150
        !                !    AFLF(L) = AMIN1(1.0,AFLF(L) + AMAX1(0.0,AFLF(0)) * (LATLPOT(L)-LATLPREV(L))/LAPOTX(L))                           !EQN 151
        !                !    IF (CFLAFLF.EQ.'N') AFLF(L) = 1.0                                                   
        !                !ENDIF     
        !                IF (LAGETT(BR,LF).LE.LLIFGTT) THEN
        !                    IF (LNUMSIMSTG(BR).LT.LCNUMX) THEN
        !                        !LPM 15NOV15 Variables LAGL3, LAGL3T and LATL3T created to save the actual leaf are by cohort (all the plant (all branches and shoots))
        !                        LAGL3(BR,LF) = LAGL(BR,LF) * AMIN1(AFLF(0,0),WFG,NFG)  
        !                        !LATL3(BR,LF)= LATL2(BR,LF)-LAGL(BR,LF) + LAGL3(BR,LF)                                             !EQN 150 !LPM  15NOV15 The reduction is just in the leaf area growing
        !                        LATL3(BR,LF)= LATL3(BR,LF) + LAGL3(BR,LF)                                             !EQN 150 !LPM 24APR2016 to keep leaf area value with stress
        !                        AFLF(BR,LF) = AMIN1(1.0,AFLF(BR,LF) + AMAX1(0.0,AFLF(0,0)) * (LAGL(BR,LF))/LAPOTX2(BR,LF))             !EQN 151   
        !                        LAGL3T(BR,LF) = LAGL3(BR,LF)*BRNUMST(BR) 
        !                        LATL3T(BR,LF) = LATL3(BR,LF)*BRNUMST(BR)
        !                        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
        !                            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
        !                                LAGL3T(BR,LF) = LAGL3T(BR,LF)+(LAGL3(BR,LF)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
        !                                LATL3T(BR,LF) = LATL3T(BR,LF)+(LATL3(BR,LF)*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
        !                            ENDIF
        !                        ENDDO
        !                        !IF (CFLAFLF.EQ.'N') AFLF(BR,LF) = 1.0                                                 !LPM 23MAR15 Define previously  
        !                    ENDIF  
        !                ENDIF
        !            ENDDO
        !        ENDDO
        ENDIF                                                                                                            !MF 21AU16 ErrorFix. Added to terminate block
        !    !PLAGSB3 = PLAGSB2 * AFLF(0)                                                                                !EQN 345
        !    !SHLAGB2 is not necessary, previously defined in the loop as SHLAG2
        !    !SHLAGB3(1) = SHLAGB2(1) * AFLF(0)                                                                          !EQN 240
        !    !SHLAGB3(2) = SHLAGB2(2) * AFLF(0)                                                                          !EQN 240
        !    !SHLAGB3(3) = SHLAGB2(3) * AFLF(0)                                                                          !EQN 240
        !    PLAGSB3 = PLAGSB2 * AFLF(0,0) 
        !    SHLAGB3(1) = SHLAG2(1) * AFLF(0,0)                                                                         !EQN 240
        !    SHLAGB3(2) = SHLAG2(2) * AFLF(0,0)                                                                         !EQN 240
        !    SHLAGB3(3) = SHLAG2(3) * AFLF(0,0)                                                                         !EQN 240    
        !ENDIF
        !    
        !-----------------------------------------------------------------------
        !           Stem and Plant. stick growth                                     
        !-----------------------------------------------------------------------
        
        GROCR = 0.0
        GROSTCRP = 0.0
        GROST = 0.0
        GROSTCR = 0.0
        STAIG = 0.0
        STAIS = 0.0
        !! Potential stem weight increase.
        !IF (SWFR.LT.1.0) THEN
        !    GROSTCRP = GROLFP * SWFR/(1.0-SWFR)                                                                        !EQN 381a
        !    GROSTCRPSTORE = AMAX1(GROLFP,GROSTCRPSTORE)                                                                !EQN 382
        !ELSE  
        !    GROSTCRP = GROSTCRPSTORE                                                                                   !EQN 381b
        !    ! LAH May need to change GROSTCRP as progress
        !ENDIF
        
        ! Potential stem weight increase. !LPM 28APR15 Change according to the new equation for stem development (see above GROSTP)
        GROSTCRP = GROSTP                                                                                          !EQN 381a
        
        
        IF (GROLFP+GROSTCRP.GT.0.0) GROSTCR = GROLS * GROSTCRP/(GROLSP) * (1.0-RSFRS)                         !EQN 383 !LPM 02OCT2015 Modified to consider GROLSP
        ! LAH RSFRS is the fraction of stem growth to reserves
        ! May need to have this change as stem growth proceeds
        
        ! Planting stick .. in balance with stem
        !GROCR = GROSTCR * GROCRFR                                                                                      !EQN 384
        !GROST = GROSTCR * (1.0-GROCRFR)                                                                                !EQN 385
        
        !LPM 20MAY2015 Planting stick grows as BR=0. It assumes an internode length of 2 cm to define the amount of nodes 
        !in the planting stick (In the future it could be modified as an input in the X-file)
        GROST = GROSTCR
        
        IF (GROLSP.GT.0.0) GROCR = GROLS*(GROCRP/GROLSP)   !LPM 05OCT2015 To avoid wrong values for GROCR            
        !CRWTP = CRWTP + GROCR                 !LPM 020CT2015 Deleted to consider before (line 320)
        
                          

        
    END SUBROUTINE CS_Growth_Part