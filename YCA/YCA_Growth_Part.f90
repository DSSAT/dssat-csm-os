!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 4707 - 5046 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Part calculates assimilation partitioning, growth of storage roots, leaves, stems and Plant. sticks.
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Growth_Part ( &
        BRSTAGE     , ISWNIT      , NFP         &
        )
    
        USE ModuleDefs
        USE YCA_First_Trans_m
    
        IMPLICIT NONE
        
        CHARACTER(LEN=1) ISWNIT      
        REAL    BRSTAGE     , NFP         

        REAL    CSYVAL      , TFAC4                                                                       ! Real function call
        
        ! Full NODEWTGB equation
        ! NDDAED = NDDAE/d
        ! NODEWTGB = 1/(1+(NDLEV/b)**c)  *  (e * (NDDAED)**f / (NDDAE * ((NDDAED**g)+1)**2))  * VF * TT
        REAL, PARAMETER :: NDLEV_B = 86.015564      ! this is 'b' from  1/(1+(NDLEV/b)**c) see issue #24
        REAL, PARAMETER :: NDLEV_C = 6.252552       ! this is 'c' from  1/(1+(NDLEV/b)**c) see issue #24
        REAL, PARAMETER :: NDDAE_D = 235.16408564   ! this is 'd' from  NDDAE/d
        REAL, PARAMETER :: NDDAE_E = 0.007610082    ! this is 'e' from  (e * (NDDAED)**f / (NDDAE * ((NDDAED**g)+1)**2))
        REAL, PARAMETER :: NDDAE_F = -2.045472      ! this is 'f' from  (e * (NDDAED)**f / (NDDAE * ((NDDAED**g)+1)**2))
        REAL, PARAMETER :: NDDAE_G = -1.045472      ! this is 'g' from  (e * (NDDAED)**f / (NDDAE * ((NDDAED**g)+1)**2))
        REAL NDDAED

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
                plant(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1))%LAPOTX =  LAXS*((DAWWP*1E-3)+0.10)                  ! LPM 07MAR15 
            ELSE
                IF (DAWWP-TT.LT.900) DALSMAX = DAE                                 ! LPM 28FEB15 to define the day with the maximum leaf size
                !LAPOTX(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1)) = LAXS/((1+(4.154582E-2*(DAE-DALSMAX))))
                !LPM 12JUL2015 test with thermal time with optimum of 20 C
                !LPM 24APR2016 Use of DALS (considering water stress) instead of TTCUMLS
                plant(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1))%LAPOTX = LAXS/((1+(5.665259E-3*(DALS))))
            ENDIF
        
                                                                                                              
            DO BR = 0, BRSTAGE                                                                                        !LPM 23MAR15 To consider cohorts
               SHLAG2B(BR) = 0.0  
                DO LF = 1, LNUMSIMSTG(BR)+1
                    IF (plant(BR,LF)%LAGETT<=LLIFGTT) THEN
                ! Basic leaf growth calculated on chronological time base. 
                ! Basic response (cm2/day) considering a maximum growing duration of 10 days 
                        plant(BR,LF)%LATLPREV = plant(BR,LF)%LATL
                        !LATLPOT(L)=LAPOTX(L)*((LAGETT(L)+TTLFLIFE*EMRGFR)/LLIFG)                                                   !EQN 322 !LPM 24APR2016 To estimate daily leaf area increase instead of total
                        plant(BR,LF)%LAPOTX2 = plant(BR,LF)%LAPOTX*TFLFLIFE
                        plant(BR,LF)%LAGL=plant(BR,LF)%LAPOTX2*(TTLFLIFE/LLIFGTT)
                        !IF (plant(BR,LF)%LAGL < 0.0) THEN !LPM 07JULY2017 Modified the order to avoid LAGL with negative values
                        !    plant(BR,LF)%LAGL = 0.0
                        !ENDIF
                        !IF (plant(BR,LF)%LAGL > (plant(BR,LF)%LAPOTX2-plant(BR,LF)%LATL3)) THEN                                           !DA IF there is something left to grow
                        !    plant(BR,LF)%LAGL = plant(BR,LF)%LAPOTX2-plant(BR,LF)%LATL3
                        !ENDIF
                        IF (plant(BR,LF)%LAGL > (plant(BR,LF)%LAPOTX2-plant(BR,LF)%LATL3)) THEN                                           !DA IF there is something left to grow
                            plant(BR,LF)%LAGL = plant(BR,LF)%LAPOTX2-plant(BR,LF)%LATL3
                        ENDIF
                        IF (plant(BR,LF)%LAGL < 0.0) THEN
                            plant(BR,LF)%LAGL = 0.0
                        ENDIF
                        !LATL(BR,LF) = LATL(BR,LF) + (LATLPOT(BR,LF)-LATLPREV(BR,LF))                                                !EQN 323 !LPM 24APR2016 To estimate daily leaf area increase instead of total
                        plant(BR,LF)%LATL = plant(BR,LF)%LATL + plant(BR,LF)%LAGL                              !Leaf area = Leaf area + leaf growth    !EQN 323
                        plant(BR,LF)%LATL = AMIN1(plant(BR,LF)%LATL, plant(BR,LF)%LAPOTX)                                                            ! DA 28OCT2016 Limiting LATL to not get over the maximum potential
                        !LATL2(l) = LATL2(L) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                         !EQN 324 LPM 21MAR15 TFG is changed by Tflflife to be able to change the Tb
                        !SHLAG2(1) = SHLAG2(1) + (LATLPOT(L)-LATLPREV(L))* AMIN1(WFG,NFG)*TFG                                       !EQN 325
                        !LAGL(BR,LF) = LATLPOT(BR,LF)* AMIN1(WFG,NFG)
                        plant(BR,LF)%LATL2 = plant(BR,LF)%LATL2 + plant(BR,LF)%LAGL                                  !EQN 324 !LPM 24APR2016 LATL2 with the same value than LATL 
                        plant(BR,LF)%LATL2 = AMIN1(plant(BR,LF)%LATL2, plant(BR,LF)%LAPOTX)                                               !DA 28OCT2016 Limiting LATL2 to not get over the maximum potential
                        SHLAG2B(BR) = SHLAG2B(BR) + plant(BR,LF)%LAGL                                    !EQN 325
                     
                        !LPM 15NOV15 Variables LAGLT and LATL2T created to save the leaf are by cohort (all the plant (all branches and shoots))
                        plant(BR,LF)%LAGLT = plant(BR,LF)%LAGL * BRNUMST(BR) ! To initialize before adding over shoots     
                        plant(BR,LF)%LATL2T = plant(BR,LF)%LATL2 * BRNUMST(BR)
                        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                            IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                                plant(BR,LF)%LAGLT = plant(BR,LF)%LAGLT+(plant(BR,LF)%LAGL*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
                                plant(BR,LF)%LATL2T = plant(BR,LF)%LATL2T+(plant(BR,LF)%LATL2*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                            ENDIF
                        ENDDO

              
               
                    ! The 2 at the end of the names indicates that 2 groups 
                    ! of stresses have been taken into account
                    ! Stress factors for individual leaves
                        IF (plant(BR,LF)%LAPOTX2 > 0.0) THEN
                            plant(BR,LF)%WFLF = AMIN1(1.0,plant(BR,LF)%WFLF+WFG*plant(BR,LF)%LAGL/plant(BR,LF)%LAPOTX2)                                        !EQN 326
                            plant(BR,LF)%NFLF = AMIN1(1.0,plant(BR,LF)%NFLF+NFG*plant(BR,LF)%LAGL/plant(BR,LF)%LAPOTX2)                                        !EQN 327
                            plant(BR,LF)%NFLFP = AMIN1(1.0,plant(BR,LF)%NFLFP+NFP*plant(BR,LF)%LAGL/plant(BR,LF)%LAPOTX2)                                      !EQN 328
                            plant(BR,LF)%TFGLF = AMIN1(1.0,plant(BR,LF)%TFGLF+TFG*plant(BR,LF)%LAGL/plant(BR,LF)%LAPOTX2)                                      !EQN 329
                            plant(BR,LF)%TFDLF = AMIN1(1.0,plant(BR,LF)%TFDLF+TFD*plant(BR,LF)%LAGL/plant(BR,LF)%LAPOTX2)                                      !EQN 330
                        ELSE
                            plant(BR,LF)%WFLF =  1.0                                   !EQN 326
                            plant(BR,LF)%NFLF =  1.0                                   !EQN 327
                            plant(BR,LF)%NFLFP = 1.0                                   !EQN 328
                            plant(BR,LF)%TFGLF = 1.0                                   !EQN 329
                            plant(BR,LF)%TFDLF = 1.0                                   !EQN 330
                        ENDIF
                        
                            ! New LEAF
                        IF (LF.EQ.LNUMSIMSTG(BR).AND.LNUMG.GT.LNUMNEED.AND.BR.EQ.BRSTAGE) THEN                                             ! This is where new leaf is initiated
                            !LAGL(BR,L+1) = LAPOTX(BR,L+1) * (TTLFLIFE*EMRGFR) * (((LNUMG-LNUMNEED)/LNUMG)/LLIFG)      ! LAGL(LNUMX)         ! Leaf area growth,shoot,lf pos  cm2/l   !EQN 331  
                            !LAGL(BR,LF+1)% = LAPOTX(BR,LF+1)% * EMRGFR * ((LNUMG-LNUMNEED)/LNUMG) * AMIN1(WFG,NFG)*Tflflife                                      !LPM 23MAR15 To define proportional growth by day      
                            plant(BR,LF+1)%LAPOTX2 = plant(BR,LF+1)%LAPOTX * TFLFLIFE
                            plant(BR,LF+1)%LAGL = plant(BR,LF+1)%LAPOTX2 * (TTLFLIFE/LLIFGTT)* EMRGFR * ((LNUMG-LNUMNEED)/LNUMG)   !LPM 02SEP2016 To register the growth of the leaf according LAGL(BR,LF) (see above)
                            IF (plant(BR,LF+1)%LAGL < 0.0) THEN 
                                plant(BR,LF+1)%LAGL = 0.0
                            ENDIF
                            plant(BR,LF+1)%LATL = plant(BR,LF+1)%LATL + plant(BR,LF+1)%LAGL                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   !EQN 333   
                            plant(BR,LF+1)%LATL2 = plant(BR,LF+1)%LATL2 + plant(BR,LF+1)%LAGL                              ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l   !EQN 334
                            SHLAG2B(BR) = SHLAG2B(BR) + plant(BR,LF+1)%LAGL                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2     !EQN 335
                            plant(BR,LF+1)%LBIRTHDAP = DAP                                                                ! LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #  
                        
                            plant(BR,LF+1)%LAGLT = plant(BR,LF+1)%LAGL*BRNUMST(BR)
                            plant(BR,LF+1)%LATL2T = plant(BR,LF+1)%LATL2*BRNUMST(BR)
                            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                                IF (SHNUM-FLOAT(L-1).GT.0.0) THEN
                                    plant(BR,LF+1)%LAGLT = plant(BR,LF+1)%LAGLT+(plant(BR,LF+1)%LAGL*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
                                    plant(BR,LF+1)%LATL2T = plant(BR,LF+1)%LATL2T+(plant(BR,LF+1)%LATL2*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                                ENDIF
                            ENDDO
                        
                            ! Stress factors for individual leaves                       
                            plant(BR,LF+1)%WFLF = AMIN1(1.0,plant(BR,LF+1)%WFLF+WFG * plant(BR,LF+1)%LAGL/plant(BR,LF+1)%LAPOTX)                                             !EQN 336
                            plant(BR,LF+1)%NFLF = AMIN1(1.0,plant(BR,LF+1)%NFLF+NFG * plant(BR,LF+1)%LAGL/plant(BR,LF+1)%LAPOTX)                                             !EQN 337
                            plant(BR,LF+1)%NFLFP = AMIN1(1.0,plant(BR,LF+1)%NFLFP+NFP * plant(BR,LF+1)%LAGL/plant(BR,LF+1)%LAPOTX)                                           !EQN 338
                            plant(BR,LF+1)%TFGLF = AMIN1(1.0,plant(BR,LF+1)%TFGLF+TFG * plant(BR,LF+1)%LAGL/plant(BR,LF+1)%LAPOTX)                                           !EQN 339
                            plant(BR,LF+1)%TFDLF = AMIN1(1.0,plant(BR,LF+1)%TFDLF+TFD * plant(BR,LF+1)%LAGL/plant(BR,LF+1)%LAPOTX)                                           !EQN 340
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
        plant%NODEWTG = 0.0
        GROSTP = 0.0
        GROCRP = 0.0
        Lcount = 0
        !IF (DAE > 0.0) THEN !LPM 01SEP16 putting a conditional DAE > 0.0 to avoid illogical values of NODEWTGB
        IF (DAG > 0.0) THEN !LPM 10JUL2017 DAG instead of DAE To consider root and stem develpment after germination and before emergence (planting stick below-ground)
          DO BR = 0, BRSTAGE               ! for each branch   
            DO LF = 1, LNUMSIMSTG(BR)    ! and each node of the branches
                Lcount = Lcount+1
                
                
          !      plant(BR,LF)%NODEWTGB = ((1/(1+(((Lcount)/NDLEV_B)**NDLEV_C)))*(0.0136142*(((DAE-plant(BR,LF)%NDDAE+1)/163.082822)**-2.81690408)/ & 
          !      (((((DAE-plant(BR,LF)%NDDAE+1)/163.082822)**-1.81690408)+1)**2))*TFG*NODWT)
          
                NDDAED=(DAG-plant(BR,LF)%NDDAE+1)/NDDAE_D
          
                !LPM23FEB2017 New high initial rate
                plant(BR,LF)%NODEWTGB = (1/(1+(((Lcount)/NDLEV_B)**NDLEV_C)))  *  (NDDAE_E*(((NDDAED)**NDDAE_F) / ((NDDAED**NDDAE_G)+1)**2))  *  TFG  * WFG *NODWT !LPM12JUL2017 adding water factor of growth
           
                plant(BR,LF)%NODEWTG = plant(BR,LF)%NODEWTGB
                !IF (BR.EQ.0.AND.LF.EQ.1.AND.DAE.EQ.1.AND.SEEDUSES.GT.0.0) NODEWTG(BR,LF) = SEEDUSES + NODEWTGB(BR) !LPM 22MAR2016 To add the increase of weight from reserves 
                plant(BR,LF)%NODEWT = plant(BR,LF)%NODEWT + plant(BR,LF)%NODEWTG
                GROSTP = GROSTP + (plant(BR,LF)%NODEWTG*BRNUMST(BR)) !LPM08JUN2015 added BRNUMST(BR) to consider the amount of branches by br. level
                STWTP = STWTP + (plant(BR,LF)%NODEWTG*BRNUMST(BR))
                
                
                ENDDO
            ENDDO
        ENDIF
        

        
        GROCRP = plant(0,1)%NODEWTGB * SPRL/NODLT   !LPM 02OCT2015 Added to consider the potential increase of the planting stick                
        CRWTP = CRWTP + GROCRP    !LPM 23MAY2015 Added to keep the potential planting stick weight
        GRORP = (GROLFP + GROSTP)*PTFA
        !GRORP = (GROLFP + GROSTP)*(0.05+0.1*EXP(-0.005*Tfgem)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        !GRORP = (GROLFP + GROSTP)*(0.05+0.1*EXP(-0.005*Tfd)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        GROLSP = GROLFP + GROSTP + GROCRP + GRORP  !LPM 02OCT2015 Added to consider the potential increase of the planting stick                                                                                    
        
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
                    CALL WARNING(3,'CSYCA',MESSAGE)
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
                CALL WARNING(1,'CSYCA',MESSAGE)
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
            IF (PLAGSB2 > AREAPOSSIBLE.AND.PLAGSB2 > 0.0)THEN
                plant(0,0)%AFLF = AREAPOSSIBLE/PLAGSB2                                                                         !EQN 149
            ELSE  
                plant(0,0)%AFLF = 1.0
            ENDIF
            IF (CFLAFLF.EQ.'N') plant(0,0)%AFLF = 1.0                                                                            !MF 21AU16 ErrorFix Added second subscript to AFLF.


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
        
        IF (GROLSP.GT.0.0) THEN
            GROCR = GROLS*(GROCRP/GROLSP)   !LPM 05OCT2015 To avoid wrong values for GROCR 
            RTWTG = GROLS*(GRORP/GROLSP) !LPM 22DEC2016 Root development 
        ENDIF
        
        !CRWTP = CRWTP + GROCR                 !LPM 020CT2015 Deleted to consider before (line 320)                      

        
    END SUBROUTINE YCA_Growth_Part