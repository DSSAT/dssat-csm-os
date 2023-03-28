!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4707 - 5046 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_Part calculates assimilation partitioning, growth of storage roots, leaves, stems and Plant. sticks.
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Growth_Part ( &
        BRSTAGE     , ISWNIT      , NFP        , LAI)

! 2023-01-25 chp removed unused variables
!   WEATHER    

        USE ModuleDefs
        USE YCA_First_Trans_m
    
        IMPLICIT NONE
        EXTERNAL WARNING
        
!       TYPE (WeatherType), intent (in) :: WEATHER    ! Defined in ModuleDefs
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        INTEGER :: Lcount                   ! counter for iterations in leafs (Lcount)
        CHARACTER(LEN=1) ISWNIT      
        REAL    BRSTAGE     , NFP      , LAI    

!       REAL    CSYVAL      , TFAC4     , TFAC5                                                           ! Real function call !LPM 19SEP2017 Added tfac5
        
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

        
        CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ))                                                                         !EQN 283
        


        LAWL(1) = LAWS - (SLATR*AMAX1(0.0,SLATS-TMEAN))*LAWS
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
        node%LAGL = 0.0
        node%LAGLT = 0.0
            
        
        

        ! Potential leaf size for next growing leaf - main shoot 
        IF (DAE > 0.0) THEN
            LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM                                                                           !EQN 332
            IF (ABS(LNUMNEED) <= 1.0E-6) LNUMNEED = 0.0
            !LPM 25/02/2015 the next lines are commented out to change the strategy to estimate the potential leaf area
        
                
            IF (DAWWP < 900) THEN
                IF (WFGREA > 1.0) THEN
                    node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX =  LAXS                  ! LPM 07MAR15
                ELSE
                    node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX =  LAXS*((DAWWP*1E-3)+0.10)
                ENDIF    
            ELSE
                IF (DAWWP-TT< 900) DALSMAX = DAE                                 ! LPM 28FEB15 to define the day with the maximum leaf size
                !LPM 12JUL2015 test with thermal time with optimum of 20 C
                !LPM 24APR2016 Use of DALS (considering water stress) instead of TTCUMLS
                IF (PDL(1) < 1200.) THEN
                    node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX = LAXS *(0.9**(BRSTAGE))/((1+(5.665259E-3*(DALS))))
                ELSE
                    node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX = LAXS /((1+(1.665259E-3*(DALS))))
                ENDIF
                
                !LPM 31MAR2021 Increase leaf size during the recovery of water stress
                IF (WFGREA > 1.0) THEN
                    node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX = 2.0 *(node(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1))%LAPOTX)
                ENDIF
                
            ENDIF
            !LPM 16sep2020 Define potential leaf size for previous leaf if two leaves are created the same day
            IF (node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)))%LAPOTX <= 0.0) THEN
                node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)))%LAPOTX = node(BRSTAGEINT,(LNUMSIMSTG(BRSTAGEINT)+1))%LAPOTX
            ENDIF
            

            node%LAGL = 0.0
            node%LAGLT = 0.0
        
                                                                                                              
            DO BR = 0, BRSTAGEINT                                                                                        !LPM 23MAR15 To consider cohorts
               SHLAG2B(BR) = 0.0  
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (node(BR,LF)%LAGETT <= LLIFGTT) THEN
                ! Basic leaf growth calculated on chronological time base. 
                ! Basic response (cm2/day) considering a maximum growing duration of 10 days 
                !!LPM 09OCT2019 Remove TFLfgrowth because it is the same than TFG 
                        node(BR,LF)%LATLPREV = node(BR,LF)%LATL
                        node(BR,LF)%LAPOTX2 = node(BR,LF)%LAPOTX*TFG
                        node(BR,LF)%LAGL=node(BR,LF)%LAPOTX2*(TTL/LLIFGTT)
                        IF (node(BR,LF)%LAGL > (node(BR,LF)%LAPOTX2-node(BR,LF)%LATL3)) THEN                                           !DA IF there is something left to grow
                            node(BR,LF)%LAGL = node(BR,LF)%LAPOTX2-node(BR,LF)%LATL3
                        ENDIF
                        IF (node(BR,LF)%LAGL < 0.0) THEN
                            node(BR,LF)%LAGL = 0.0
                        ENDIF
                        node(BR,LF)%LATL = node(BR,LF)%LATL + node(BR,LF)%LAGL                              !Leaf area = Leaf area + leaf growth    !EQN 323
                        node(BR,LF)%LATL = AMIN1(node(BR,LF)%LATL, node(BR,LF)%LAPOTX)                                                            ! DA 28OCT2016 Limiting LATL to not get over the maximum potential
                        node(BR,LF)%LATL2 = node(BR,LF)%LATL2 + node(BR,LF)%LAGL                                  !EQN 324 !LPM 24APR2016 LATL2 with the same value than LATL 
                        node(BR,LF)%LATL2 = AMIN1(node(BR,LF)%LATL2, node(BR,LF)%LAPOTX)                                               !DA 28OCT2016 Limiting LATL2 to not get over the maximum potential
                        SHLAG2B(BR) = SHLAG2B(BR) + node(BR,LF)%LAGL                                    !EQN 325
                     
                        !LPM 15NOV15 Variables LAGLT and LATL2T created to save the leaf are by cohort (all the plant (all branches and shoots))
                        node(BR,LF)%LAGLT = node(BR,LF)%LAGL * BRNUMST(BR) ! To initialize before adding over shoots     
                        node(BR,LF)%LATL2T = node(BR,LF)%LATL2 * BRNUMST(BR)
                        DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                            IF (SHNUM-FLOAT(L-1) > 0.0) THEN
                                node(BR,LF)%LAGLT = node(BR,LF)%LAGLT+(node(BR,LF)%LAGL*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
                                node(BR,LF)%LATL2T = node(BR,LF)%LATL2T+(node(BR,LF)%LATL2*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                            ENDIF
                        ENDDO

              
               
                    ! The 2 at the end of the names indicates that 2 groups 
                    ! of stresses have been taken into account
                    ! Stress factors for individual leaves
                        IF (node(BR,LF)%LAPOTX2 > 0.0) THEN
                            node(BR,LF)%WFLF =  AMIN1(1.0, node(BR,LF)%WFLF+WFG  * node(BR,LF)%LAGL/node(BR,LF)%LAPOTX2)                                        !EQN 326
                            node(BR,LF)%NFLF =  AMIN1(1.0, node(BR,LF)%NFLF+NFG  * node(BR,LF)%LAGL/node(BR,LF)%LAPOTX2)                                        !EQN 327
                            node(BR,LF)%NFLFP = AMIN1(1.0, node(BR,LF)%NFLFP+NFP * node(BR,LF)%LAGL/node(BR,LF)%LAPOTX2)                                      !EQN 328
                            node(BR,LF)%TFGLF = AMIN1(1.0, node(BR,LF)%TFGLF+TFG * node(BR,LF)%LAGL/node(BR,LF)%LAPOTX2)                                      !EQN 329
                            node(BR,LF)%TFDLF = AMIN1(1.0, node(BR,LF)%TFDLF+TFD * node(BR,LF)%LAGL/node(BR,LF)%LAPOTX2)                                      !EQN 330
                        ELSE
                            node(BR,LF)%WFLF =  1.0                                   !EQN 326
                            node(BR,LF)%NFLF =  1.0                                   !EQN 327
                            node(BR,LF)%NFLFP = 1.0                                   !EQN 328
                            node(BR,LF)%TFGLF = 1.0                                   !EQN 329
                            node(BR,LF)%TFDLF = 1.0                                   !EQN 330
                        ENDIF
                        
                            ! New LEAF
                        IF (LF == LNUMSIMSTG(BR) .AND. LNUMG > LNUMNEED .AND. BR == BRSTAGEINT) THEN                                             ! This is where new leaf is initiated
                            node(BR,LF+1)%LAPOTX2 = node(BR,LF+1)%LAPOTX * TFG
                            !LPM 09OCT2019 Remove TTLfgrowth because it is the same than TFG 
                            node(BR,LF+1)%LAGL = node(BR,LF+1)%LAPOTX2 * (TTL/LLIFGTT)* EMRGFR * ((LNUMG-LNUMNEED)/LNUMG)   !LPM 02SEP2016 To register the growth of the leaf according LAGL(BR,LF) (see above)
                            IF (node(BR,LF+1)%LAGL < 0.0) THEN 
                                node(BR,LF+1)%LAGL = 0.0
                            ENDIF
                            node(BR,LF+1)%LATL = node(BR,LF+1)%LATL + node(BR,LF+1)%LAGL                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   !EQN 333   
                            node(BR,LF+1)%LATL2 = node(BR,LF+1)%LATL2 + node(BR,LF+1)%LAGL                              ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l   !EQN 334
                            SHLAG2B(BR) = SHLAG2B(BR) + node(BR,LF+1)%LAGL                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2     !EQN 335
                            node(BR,LF+1)%LBIRTHDAP = DAP                                                                ! LBIRTHDAP(LCNUMX)   ! DAP on which leaf initiated #  
                        
                            node(BR,LF+1)%LAGLT = node(BR,LF+1)%LAGL*BRNUMST(BR)
                            node(BR,LF+1)%LATL2T = node(BR,LF+1)%LATL2*BRNUMST(BR)
                            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                                IF (SHNUM-FLOAT(L-1) > 0.0) THEN
                                    node(BR,LF+1)%LAGLT = node(BR,LF+1)%LAGLT+(node(BR,LF+1)%LAGL*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))
                                    node(BR,LF+1)%LATL2T = node(BR,LF+1)%LATL2T+(node(BR,LF+1)%LATL2*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                                ENDIF
                            ENDDO
                        
                            ! Stress factors for individual leaves                       
                            node(BR,LF+1)%WFLF = AMIN1(1.0,node(BR,LF+1)%WFLF+WFG * node(BR,LF+1)%LAGL/node(BR,LF+1)%LAPOTX)                                             !EQN 336
                            node(BR,LF+1)%NFLF = AMIN1(1.0,node(BR,LF+1)%NFLF+NFG * node(BR,LF+1)%LAGL/node(BR,LF+1)%LAPOTX)                                             !EQN 337
                            node(BR,LF+1)%NFLFP = AMIN1(1.0,node(BR,LF+1)%NFLFP+NFP * node(BR,LF+1)%LAGL/node(BR,LF+1)%LAPOTX)                                           !EQN 338
                            node(BR,LF+1)%TFGLF = AMIN1(1.0,node(BR,LF+1)%TFGLF+TFG * node(BR,LF+1)%LAGL/node(BR,LF+1)%LAPOTX)                                           !EQN 339
                            node(BR,LF+1)%TFDLF = AMIN1(1.0,node(BR,LF+1)%TFDLF+TFD * node(BR,LF+1)%LAGL/node(BR,LF+1)%LAPOTX)                                           !EQN 340
                        ENDIF
                    ENDIF
                ENDDO
            SHLAG2(1) = SHLAG2(1)+(SHLAG2B(BR)*BRNUMST(BR))                                                                                            !LPM 23MAR15 To have the leaf area fro one shoot considering all the branches (axis)
            ENDDO
 
            ! Leaf area increase:growing leaves on 1 axis,all shoots
            PLAGSB2 = SHLAG2(1) ! To initialize before adding over shoots                                                  
            DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                IF (SHNUM-FLOAT(L-1) > 0.0) THEN
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
            IF (LAWL(1) > 0.0) THEN
                GROLFP = (PLAGSB2/LAWL(1)) / (1.0-LPEFR)                                                   !EQN 297    
            ENDIF
        ENDIF
        
        
        !LPM 11APR15  Rate of node weight increase by branch level and cohort  
        node%NODEWTG = 0.0
        GROSTP = 0.0
        GROCRP = 0.0
        Lcount = 0
        !IF (DAE > 0.0) THEN !LPM 01SEP16 putting a conditional DAE > 0.0 to avoid illogical values of NODEWTGB
        IF (DAG > 0.0) THEN !LPM 10JUL2017 DAG instead of DAE To consider root and stem develpment after germination and before emergence (planting stick below-ground)
          DO BR = 0, BRSTAGEINT               ! for each branch   
            DO LF = 1, LNUMSIMSTG(BR)    ! and each node of the branches
                Lcount = Lcount+1

          
                NDDAED=(DAG-node(BR,LF)%NDDAE+1)/NDDAE_D
          
                !LPM23FEB2017 New high initial rate
                !LPM12JUL2017 adding water factor of growth
                !LPM 09OCT2019 Removing the water stress factor to be considered at the same time  than the assimilates and the N restrictions
                node(BR,LF)%NODEWTGB = (1/(1+(((Lcount)/NDLEV_B)**NDLEV_C)))  *  (NDDAE_E*(((NDDAED)**NDDAE_F) / ((NDDAED**NDDAE_G)+1)**2))  *  TFG *NODWT 
           
                node(BR,LF)%NODEWTG = node(BR,LF)%NODEWTGB
                !IF (BR == 0.AND.LF == 1.AND.DAE == 1.AND.SEEDUSES > 0.0) NODEWTG(BR,LF) = SEEDUSES + NODEWTGB(BR) !LPM 22MAR2016 To add the increase of weight from reserves 
                node(BR,LF)%NODEWT = node(BR,LF)%NODEWT + node(BR,LF)%NODEWTG
                GROSTP = GROSTP + (node(BR,LF)%NODEWTG*BRNUMST(BR)) !LPM08JUN2015 added BRNUMST(BR) to consider the amount of branches by br. level
                STWTP = STWTP + (node(BR,LF)%NODEWTG*BRNUMST(BR))
                
                
                ENDDO
            ENDDO
        ENDIF
        
!       chp 2019-04-10 added zero divide protection
        if (nodlt > 1.e-6) then
          GROCRP = node(0,1)%NODEWTGB * SPRL/NODLT   !LPM 02OCT2015 Added to consider the potential increase of the planting stick                
        else
          GROCRP = 0.0
        endif

        CRWTP = CRWTP + GROCRP    !LPM 23MAY2015 Added to keep the potential planting stick weight
        GRORP = (GROLFP + GROSTP + GROCRP)*PTFA !LPM 08OCT2019 Added to consider planting stick weight
        !GRORP = (GROLFP + GROSTP)*(0.05+0.1*EXP(-0.005*Tfgem)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        !GRORP = (GROLFP + GROSTP)*(0.05+0.1*EXP(-0.005*Tfd)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        GROLSP = GROLFP + GROSTP + GROCRP + GRORP  !LPM 02OCT2015 Added to consider the potential increase of the planting stick                                                                                    
        
        IF (GROLSP > 0.0) THEN
            ! Leaf+stem weight increase from assimilates
            !GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT-GROSR))                                                              !EQN 298 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
            GROLSA = AMAX1(0.,AMIN1(GROLSP,CARBOT))                                                                     !EQN 298
            ! Leaf+stem weight increase from senescence 
            IF (GROLSA < GROLSP) THEN

                GROLSSEN = AMIN1(GROLSP-GROLSA,SENLFGRS)
            ENDIF
            
            IF (GROLSA+GROLSSEN < GROLSP) THEN
                ! Leaf+stem weight increase from seed reserves
                ! LAH May need to restrict seed use.To use by roots?
                IF (DAE <= 0.0)THEN !LPM 23MAR2016 Before emergence use the SEEDUSED according with CS_growth_Init.f90
                    GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDUSED)                                                     !EQN 300
                ELSE
                    GROLSSD = AMIN1((GROLSP-GROLSA-GROLSSEN),SEEDRSAV)                                                     !EQN 300
                    SEEDRSAV = SEEDRSAV - GROLSSD                                                                          !EQN 288
                ENDIF
               
                
                IF ( LAI <= 0.0.AND.GROLSSD <= 0.0.AND.SEEDRSAV <= 0.0.AND.ESTABLISHED /= 'Y') THEN
                    CFLFAIL = 'Y'
                    WRITE (Message(1),'(A41)') 'No seed reserves to initiate leaf growth '
                    WRITE (Message(2),'(A33,F8.3,F6.1)') '  Initial seed reserves,seedrate ',seedrsi,sdrate
                    WRITE (Message(3),'(A33,F8.3,F6.1)') '  Reserves %,plant population    ',sdrs,pltpop    !LPM 22MAR2016 Keep value SDRS  
                    CALL WARNING(3,'CSYCA',MESSAGE)
                ENDIF
            ENDIF
            ! Leaf+stem weight increase from plant reserves
            IF (GROLSA+GROLSSD+GROLSSEN < GROLSP) THEN
                GROLSRS =  AMIN1(RSWT,GROLSP-GROLSA-GROLSSD-GROLSSEN)                                            !EQN 301
            ENDIF
            

            
            ! Leaf+stem weight increase from roots (after drought)
            GROLSRT = 0.0
            GROLSRTN = 0.0
            IF ((GROLSA+GROLSSD+GROLSRS+GROLSSEN) < GROLSP.AND.SHRTD < 1.0.AND.RTUFR > 0.0.AND.ESTABLISHED == 'Y') THEN
                GROLSRT = AMIN1(RTWT*RTUFR,(GROLSP-GROLSA-GROLSSD-GROLSSEN-GROLSRS))                                   !EQN 302
                IF (ISWNIT /= 'N') THEN
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
            IF ((GROLSP) > 0.0) THEN
                GROLF = GROLS * GROLFP/GROLSP                                                                          !EQN 304
            ELSE  
                GROLF = 0.0
            ENDIF
            ! Check if enough assimilates to maintain SLA within limits
            !AREAPOSSIBLE = GROLF*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))                                                     !EQN 148 !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA 
            AREAPOSSIBLE = GROLF*(1.0-LPEFR)*LAWL(1)
            ! If not enough assim.set assimilate factor
            IF (PLAGSB2 > AREAPOSSIBLE.AND.PLAGSB2 > 0.0)THEN
                node(0,0)%AFLF = AREAPOSSIBLE/PLAGSB2                                                                         !EQN 149
            ELSE  
                node(0,0)%AFLF = 1.0
            ENDIF
            IF (CFLAFLF == 'N') node(0,0)%AFLF = 1.0                                                                            !MF 21AU16 ErrorFix Added second subscript to AFLF.


        ENDIF                                                                                                            !MF 21AU16 ErrorFix. Added to terminate block

        !-----------------------------------------------------------------------
        !           Stem and Plant. stick growth                                     
        !-----------------------------------------------------------------------
        
        GROCR = 0.0
        GROSTCRP = 0.0
        GROST = 0.0
        GROSTCR = 0.0
        STAIG = 0.0
        STAIS = 0.0
        GROSTCRP = GROSTP                                                                                          !EQN 381a
        
        !LPM 09OCT2019 Remove the reserve fraction to the stems (RSFRS)
        IF (GROLFP+GROSTCRP > 0.0) THEN
            GROSTCR = GROLS * GROSTCRP/(GROLSP)                         !EQN 383 !LPM 02OCT2015 Modified to consider GROLSP
        ENDIF

        GROST = GROSTCR
        
        IF (GROLSP > 0.0) THEN
            GROCR = GROLS*(GROCRP/GROLSP)   !LPM 05OCT2015 To avoid wrong values for GROCR 
            RTWTG = GROLS*(GRORP/GROLSP) !LPM 22DEC2016 Root development 
        ENDIF
        
        !CRWTP = CRWTP + GROCR                 !LPM 020CT2015 Deleted to consider before (line 320)                      
 
        
        GRORSP = CARBOT+GROLSSD+GROLSRT+SENLFGRS+GROLSRS-GROLF-GROST-GROCR                                    
        IF(GRORSP < 0.0.AND.GRORSP > -1.0E-07) GRORSP = 0.0

        ! Reserves to STORAGE ROOT if conc too great (overflow!)
        SRWTGRSP = 0.0
        
        SRWTGRSP = GRORSP
    END SUBROUTINE YCA_Growth_Part