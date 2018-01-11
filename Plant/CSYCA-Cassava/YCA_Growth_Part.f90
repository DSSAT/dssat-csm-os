!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 4707 - 5046 of the original CSCAS code. The names of the 
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
        USE YCA_Control_Leaf
        
        IMPLICIT NONE
        
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        REAL    :: Lcount                   ! counter for iterations in leafs (Lcount)
        CHARACTER(LEN=1) ISWNIT      
        REAL    BRSTAGE     , NFP         

        REAL    CSYVAL      , TFAC4     , TFAC5                                                           ! Real function call !LPM 19SEP2017 Added tfac5
        
        ! Full CohortWeightGrowth equation
        ! D_NewNodeDAE = NewNodeDAE/D_
        ! CohortWeightGrowth = 1/(1+(NDLEV/D_)**C_)  *  (E_ * (D_NewNodeDAE)**F_ / (NewNodeDAE * ((D_NewNodeDAE**G_)+1)**2))  * VF * TT
        REAL, PARAMETER :: B_ = 86.015564      ! this is 'b' from  1/(1+(NDLEV/b)**c) see issue #24
        REAL, PARAMETER :: C_ = 6.252552       ! this is 'c' from  1/(1+(NDLEV/b)**c) see issue #24
        REAL, PARAMETER :: D_ = 235.16408564   ! this is 'd' from  NDDAE/d
        REAL, PARAMETER :: E_ = 0.007610082    ! this is 'e' from  (e * (D_NewNodeDAE)**f / (NDDAE * ((D_NewNodeDAE**g)+1)**2))
        REAL, PARAMETER :: F_ = -2.045472      ! this is 'f' from  (e * (D_NewNodeDAE)**f / (NDDAE * ((D_NewNodeDAE**g)+1)**2))
        REAL, PARAMETER :: G_ = -1.045472      ! this is 'g' from  (e * (D_NewNodeDAE)**f / (NDDAE * ((D_NewNodeDAE**g)+1)**2))
        REAL D_NewNodeDAE
        

        !-----------------------------------------------------------------------
        !           Partitioning of C to above ground and roots (minimum) 
        !-----------------------------------------------------------------------

        
        CARBOT = AMAX1(0.0,(CARBOBEG+CARBOADJ))                                                                         !EQN 283
        


        LAWL(1) = LAWS
        !-----------------------------------------------------------------------
        !           Leaf growth
        !-----------------------------------------------------------------------

        
        LeafGrowth = 0.0
        LeafGrowthADJ = 0.0
        LeafGrowthP = 0.0
        StemLeafGrowthRS = 0.0
        StemLeafGrowth = 0.0
        StemLeafGrowthA = 0.0
        StemLeafGrowthP = 0.0
        StemLeafGrowthRT = 0.0
        StemLeafGrowthSEN = 0.0
        StemLeafGrowthRTN = 0.0
        StemLeafGrowthSD = 0.0
        !LAGEG = 0.0   !LPM 28MAR15 This variable is not used
        !PLAGS2 = 0.0  !LPM 23MAR15 non necessary PLAGSB2 considers all the branches and shoots
        SHLAG2 = 0.0
        SHLAG2B = 0.0  !LPM 23MAR15 add new variable
        SHLAGB2 = 0.0
        SHLAGB3 = 0.0
        SHLAGB4 = 0.0
            
        
        

        ! Potential leaf size for next growing leaf - main shoot 
        IF (DAE > 0.0) THEN
            LNUMNEED = FLOAT(INT(LNUM+1)) - LNUM                                                                           !EQN 332
            IF (ABS(LNUMNEED) <= 1.0E-6) LNUMNEED = 0.0
            !LPM 25/02/2015 the next lines are commented out to change the strategy to estimate the potential leaf area
        
                
            IF (DAWWP < 900) THEN
                node(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1))%LAPOTX =  LAXS*((DAWWP*1E-3)+0.10)                  ! LPM 07MAR15 
            ELSE
                IF (DAWWP-TT< 900) DALSMAX = DAE                                 ! LPM 28FEB15 to define the day with the maximum leaf size
                !LPM 12JUL2015 test with thermal time with optimum of 20 C
                !LPM 24APR2016 Use of DALS (considering water stress) instead of TTCUMLS
                node(BRSTAGE,(LNUMSIMSTG(BRSTAGE)+1))%LAPOTX = LAXS/((1+(5.665259E-3*(DALS))))
            ENDIF
        
                                                                                                              
            DO BR = 0, BRSTAGE                                                                                        !LPM 23MAR15 To consider cohorts
               SHLAG2B(BR) = 0.0  
                DO LF = 1, LNUMSIMSTG(BR)+1
                    IF (isLeafExpanding(node(BR,LF))) THEN
                ! Basic leaf growth calculated on chronological time base. 
                ! Basic response (cm2/day) considering a maximum growing duration of 10 days 
                        node(BR,LF)%LAPOTX2 = node(BR,LF)%LAPOTX*Tflfgrowth
                        node(BR,LF)%LAGL=node(BR,LF)%LAPOTX2*(TTlfgrowth/LLIFGTT)
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
                        
                            ! New Leaf
                        IF (LF == LNUMSIMSTG(BR) .AND. LNUMG > LNUMNEED .AND. BR == BRSTAGE) THEN                                             ! This is where new leaf is initiated
                            node(BR,LF+1)%LAPOTX2 = node(BR,LF+1)%LAPOTX * Tflfgrowth
                            node(BR,LF+1)%LAGL = node(BR,LF+1)%LAPOTX2 * (TTlfgrowth/LLIFGTT)* EMRGFR * ((LNUMG-LNUMNEED)/LNUMG)   !LPM 02SEP2016 To register the growth of the leaf according LAGL(BR,LF) (see above)
                            IF (node(BR,LF+1)%LAGL < 0.0) THEN 
                                node(BR,LF+1)%LAGL = 0.0
                            ENDIF
                            node(BR,LF+1)%LATL = node(BR,LF+1)%LATL + node(BR,LF+1)%LAGL                                                   ! LATL(LNUMX)         ! Leaf area,shoot,lf#,potential  cm2/l   !EQN 333   
                            node(BR,LF+1)%LATL2 = node(BR,LF+1)%LATL2 + node(BR,LF+1)%LAGL                              ! LATL2(LNUMX)        ! Leaf area,shoot,lf#,+h2o,n,tem cm2/l   !EQN 334
                            SHLAG2B(BR) = SHLAG2B(BR) + node(BR,LF+1)%LAGL                              ! SHLAG2(25)          ! Shoot lf area gr,1 axis,H2oNt  cm2     !EQN 335
                        
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
                LeafGrowthP = (PLAGSB2/LAWL(1)) / (1.0-LPEFR)                                                   !EQN 297    
            ENDIF
        ENDIF
        
        
        !LPM 11APR15  Rate of node weight increase by branch level and cohort  
        node%CohortWeightGrowth = 0.0
        StemGrowthP = 0.0
        StickGrowthP = 0.0
        Lcount = 0
        !IF (DAE > 0.0) THEN !LPM 01SEP16 putting a conditional DAE > 0.0 to avoid illogical values of CohortWeightGrowth
        IF (DAG > 0.0) THEN !LPM 10JUL2017 DAG instead of DAE To consider root and stem develpment after germination and before emergence (planting stick below-ground)
          DO BR = 0, BRSTAGE               ! for each branch   
            DO LF = 1, LNUMSIMSTG(BR)    ! and each node of the branches
                Lcount = Lcount+1
           
                 
          
                D_NewNodeDAE=(DAG-node(BR,LF)%NewNodeDAE+1)/D_
          
                !LPM23FEB2017 New high initial rate
                node(BR,LF)%CohortWeightGrowth = (1/(1+(((Lcount)/B_)**C_)))  *  (E_*(((D_NewNodeDAE)**F_) / ((D_NewNodeDAE**G_)+1)**2))  *  TFG  * WFG *NODWT !LPM12JUL2017 adding water factor of growth
                node(BR,LF)%CohortWeight = node(BR,LF)%CohortWeight + node(BR,LF)%CohortWeightGrowth
                
                StemGrowthP = StemGrowthP + (node(BR,LF)%CohortWeightGrowth*BRNUMST(BR)) !LPM08JUN2015 added BRNUMST(BR) to consider the amount of branches by br. level
                StemWeightP = StemWeightP + (node(BR,LF)%CohortWeightGrowth*BRNUMST(BR))
                
                ENDDO
            ENDDO
        ENDIF
        

        
        StickGrowthP = node(0,1)%CohortWeightGrowth * SPRL/NODLT   !LPM 02OCT2015 Added to consider the potential increase of the planting stick                
        CRWTP = CRWTP + StickGrowthP    !LPM 23MAY2015 Added to keep the potential planting stick weight
        RootGrowthP = (LeafGrowthP + StemGrowthP)*PTFA
        !RootGrowthP = (LeafGrowthP + StemGrowthP)*(0.05+0.1*EXP(-0.005*Tfgem)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        !RootGrowthP = (LeafGrowthP + StemGrowthP)*(0.05+0.1*EXP(-0.005*Tfd)) !LPM 09JAN2017 Matthews & Hunt, 1994 (GUMCAS)
        StemLeafGrowthP = LeafGrowthP + StemGrowthP + StickGrowthP + RootGrowthP  !LPM 02OCT2015 Added to consider the potential increase of the planting stick                                                                                    
        
        IF (StemLeafGrowthP > 0.0) THEN
            ! Leaf+stem weight increase from assimilates
            !StemLeafGrowthA = AMAX1(0.,AMIN1(StemLeafGrowthP,CARBOT-GROSR))                                                              !EQN 298 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
            StemLeafGrowthA = AMAX1(0.,AMIN1(StemLeafGrowthP,CARBOT))                                                                     !EQN 298
            ! Leaf+stem weight increase from senescence 
            IF (StemLeafGrowthA < StemLeafGrowthP) THEN

                StemLeafGrowthSEN = AMIN1(StemLeafGrowthP-StemLeafGrowthA,SENLFGRS)
            ENDIF
            
            IF (StemLeafGrowthA+StemLeafGrowthSEN < StemLeafGrowthP) THEN
                ! Leaf+stem weight increase from seed reserves
                ! LAH May need to restrict seed use.To use by roots?
                IF (DAE <= 0.0)THEN !LPM 23MAR2016 Before emergence use the SEEDUSED according with CS_growth_Init.f90
                    StemLeafGrowthSD = AMIN1((StemLeafGrowthP-StemLeafGrowthA-StemLeafGrowthSEN),SEEDUSED)                                                     !EQN 300
                ELSE
                    StemLeafGrowthSD = AMIN1((StemLeafGrowthP-StemLeafGrowthA-StemLeafGrowthSEN),SEEDRSAV)                                                     !EQN 300
                    SEEDRSAV = SEEDRSAV - StemLeafGrowthSD                                                                          !EQN 288
                ENDIF
               
                
                IF ( LAI <= 0.0.AND.StemLeafGrowthSD <= 0.0.AND.SEEDRSAV <= 0.0.AND.ESTABLISHED /= 'Y') THEN
                    CFLFAIL = 'Y'
                    WRITE (Message(1),'(A41)') 'No seed reserves to initiate leaf growth '
                    WRITE (Message(2),'(A33,F8.3,F6.1)') '  Initial seed reserves,seedrate ',seedrsi,sdrate
                    WRITE (Message(3),'(A33,F8.3,F6.1)') '  Reserves %,plant population    ',sdrs,pltpop    !LPM 22MAR2016 Keep value SDRS  
                    CALL WARNING(3,'CSYCA',MESSAGE)
                ENDIF
            ENDIF
            ! Leaf+stem weight increase from plant reserves
            IF (StemLeafGrowthA+StemLeafGrowthSD+StemLeafGrowthSEN < StemLeafGrowthP) THEN
                StemLeafGrowthRS =  AMIN1(RSWT*RSUSE,StemLeafGrowthP-StemLeafGrowthA-StemLeafGrowthSD-StemLeafGrowthSEN)                                            !EQN 301
            ENDIF
            ! Leaf+stem weight increase from roots (after drought)
            StemLeafGrowthRT = 0.0
            StemLeafGrowthRTN = 0.0
            IF ((StemLeafGrowthA+StemLeafGrowthSD+StemLeafGrowthRS+StemLeafGrowthSEN) < StemLeafGrowthP.AND.SHRTD < 1.0.AND.RTUFR > 0.0.AND.ESTABLISHED == 'Y') THEN
                StemLeafGrowthRT = AMIN1(RTWT*RTUFR,(StemLeafGrowthP-StemLeafGrowthA-StemLeafGrowthSD-StemLeafGrowthSEN-StemLeafGrowthRS))                                   !EQN 302
                IF (ISWNIT /= 'N') THEN
                    StemLeafGrowthRTN = StemLeafGrowthRT * RANC                                                                          !EQN 244
                ELSE
                    StemLeafGrowthRTN = 0.0
                ENDIF
                WRITE(Message(1),'(A16,A12,F3.1,A8,F7.4,A7,F7.4,A9,F7.4)') &
                    'Roots -> leaves ',' Shoot/root ',shrtd,' StemLeafGrowthP ',StemLeafGrowthP,' StemLeafGrowth ',StemLeafGrowth,' StemLeafGrowthRT ',StemLeafGrowthRT
                CALL WARNING(1,'CSYCA',MESSAGE)
            ENDIF
            ! Leaf+stem weight increase from all sources
            StemLeafGrowth = StemLeafGrowthA + StemLeafGrowthSEN + StemLeafGrowthSD + StemLeafGrowthRS + StemLeafGrowthRT                                                      !EQN 303
            ! Leaf weight increase from all sources
            IF ((StemLeafGrowthP) > 0.0) THEN
                LeafGrowth = StemLeafGrowth * LeafGrowthP/StemLeafGrowthP                                                                          !EQN 304
            ELSE  
                LeafGrowth = 0.0
            ENDIF
            ! Check if enough assimilates to maintain SLA within limits
            !AREAPOSSIBLE = LeafGrowth*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))                                                     !EQN 148 !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA 
            AREAPOSSIBLE = LeafGrowth*(1.0-LPEFR)*LAWL(1)
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
        
        StickGrowth = 0.0
        StemStickGrowthP = 0.0
        StemGrowth = 0.0
        StemStickGrowth = 0.0
        STAIG = 0.0
        STAIS = 0.0
        StemStickGrowthP = StemGrowthP                                                                                          !EQN 381a
        
        
        IF (LeafGrowthP+StemStickGrowthP > 0.0) THEN
            StemStickGrowth = StemLeafGrowth * StemStickGrowthP/(StemLeafGrowthP) * (1.0-RSFRS)                         !EQN 383 !LPM 02OCT2015 Modified to consider StemLeafGrowthP
        ENDIF

        StemGrowth = StemStickGrowth
        
        IF (StemLeafGrowthP > 0.0) THEN
            StickGrowth = StemLeafGrowth*(StickGrowthP/StemLeafGrowthP)   !LPM 05OCT2015 To avoid wrong values for StickGrowth 
            RootGrowth = StemLeafGrowth*(RootGrowthP/StemLeafGrowthP) !LPM 22DEC2016 Root development 
        ENDIF
        
        !CRWTP = CRWTP + StickGrowth                 !LPM 020CT2015 Deleted to consider before (line 320)                      

        
    END SUBROUTINE YCA_Growth_Part