!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == RATE) lines 5047 - 5350 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Growth_NUptake calculates N uptake and distribution and adjusts growth if N inadequate.
!***************************************************************************************************************************

    SUBROUTINE YCA_Growth_NUptake ( &
        BD          , DLAYR       , DUL         , ISWNIT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , &
        RLV         , SAT         , SW          , UNH4        , UNO3        , BRSTAGE     & 
        )
    
        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        USE YCA_Control_Leaf
    
        IMPLICIT NONE
        
    
        CHARACTER(LEN=1) ISWNIT      
        INTEGER NLAYR       
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        REAL    BD(NL)      , DLAYR(NL)   , DUL(NL)     , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , RLV(NL)     , SAT(NL)     
        REAL    SW(NL)      , UNH4(NL)    , UNO3(NL)    , BRSTAGE
    
        
        !-----------------------------------------------------------------------
        !           N Uptake  -  and growth adjustments if N inadequate
        !-----------------------------------------------------------------------

        ! Set adjusted values to unadjusted 
        ! For when no adjustment necessary, or when not simulating N
        !GROCRADJ = GROCR
        !GROLFADJ = GROLF
        !GROSTADJ = GROST
        !PLAGSB4 = PLAGSB3
        !RSSRWTGLFADJ = 0.0
        !RTRESPADJ = RTRESP   
        !RTWTGADJ = RTWTG
        !SHLAGB4 = SHLAGB3
        
        GROCRADJ = GROCRP
        GROLFADJ = GROLFP
        GROSTADJ = GROSTP
        PLAGSB4 = PLAGSB2
        RSSRWTGLFADJ = 0.0
        RTRESPADJ = RTRESP   
        RTWTGADJ = RTWTG
        SHLAGB4 = SHLAGB2

        node(0,0)%NFLF2 = 1.0
        
        IF (SUM(node%NODEWTG) > 0 .AND. ISWNIT /= 'N') THEN    !If plant nodes has weight and nitrogen restrictions are activated
    
            ANDEM = 0.0
            RNDEM = 0.0
            LNDEM = 0.0
            SNDEM = 0.0
            SRNDEM = 0.0
            RNDEMG = 0.0
            LNDEMG = 0.0
            SNDEMG = 0.0
            SRNDEMG = 0.0
            RNDEMTU = 0.0
            LNDEMTU = 0.0
            SNDEMTU = 0.0
            SRNDEMTU = 0.0
            SEEDNUSE = 0.0
            SEEDNUSE2 = 0.0
            RSNUSED = 0.0
    
            SNO3PROFILE = 0.0
            SNH4PROFILE = 0.0
            SNO3ROOTZONE = 0.0
            SNH4ROOTZONE = 0.0
            TRLV = 0.0
            DO L = 1, NLAYR
                TRLV = TRLV + RLV(L)
                IF((BD(L)*DLAYR(L)) > 0.0) THEN
                    FAC(L) = 10.0/(BD(L)*DLAYR(L))                                                                         !EQN 169
                    SNO3(L) = NO3LEFT(L) / FAC(L)                                                                          !EQN 170
                    SNH4(L) = NH4LEFT(L) / FAC(L)                                                                          !EQN 186
                ENDIF
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L) > 0.0) THEN
                    SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                    SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
            END DO
    
            LNDEM = GROLFP*LNCX + (LFWT-leafTotalSenescedWeight())*AMAX1(0.0,NTUPF*(LNCX-LANC)) - GROLSRTN                        !EQN 152
            !SNDEM = AMAX1(0.0,GROST+GROCR)*SNCX + (woodyWeight())*AMAX1(0.0,NTUPF*(SNCX-SANC))                             !EQN 153
            RNDEM = RTWTG*RNCX + (RTWT-SENRTG-GROLSRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))                                    !EQN 154
            
            SRNDEM = (SRWTGRS)*(SRNPCS/100.0) + SRWT*AMAX1(0.0,NTUPF*((SRNPCS/100.0)-SRANC))                     !EQN 155
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N demand by node according with its age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (GROSTP>0.0) THEN
                        node(BR,LF)%SNDEMN = AMAX1(0.0,node(BR,LF)%NODEWTG)*node(BR,LF)%SNCX + &
                        (node(BR,LF)%NODEWT*AMAX1(0.0,NTUPF*(node(BR,LF)%SNCX-node(BR,LF)%SANC)))  
                        SNDEM = SNDEM + node(BR,LF)%SNDEMN
                    ENDIF
                ENDDO
            ENDDO
            ! Seed use if no roots
            ! N use same % of initial as for CH20,if needed.
            IF (STDAY /= 0) THEN
                IF (RTWT <= 0.0) THEN
                    SEEDNUSE = AMAX1(0.0, AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))                          !EQN 203a
                ELSE
                    ! Some use of seed (0.5 need) even if may not be needed
                    SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN, 0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))                    !EQN 203b
                ENDIF
            ENDIF
    
            ! Reserves used before uptake
            RSNUSED = AMIN1(LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE,RSN)                                                     !EQN 156
    
            ! N uptake needed 
            ANDEM = AMAX1(0.0,plantPopulation()* (LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSED))                                !EQN 158
            ! Uptake
    
            ! Original from CSM with some 'modification'.  
            ! RNUMX = RTNO3,RTNH4 = N uptake/root length (mgN/cm,.006)
            ! RNO3U,RNH4U  = Nitrogen uptake (kg N/ha)
            ! RNUMX = 0.006    
            WFNU = 1.0
            NUPAP = 0.0
            RNO3U = 0.0
            RNH4U = 0.0
            TVR1 = -0.08  ! Could be AMAX1(0.01,(0.20-NconcU*0.004))
            DO L=1,NLAYR
                IF (RLV(L) > 0.0) THEN
                    NLAYRROOT = L
                    ! N concentration effects
                    FNH4 = 1.0-EXP(TVR1*NH4CF * NH4Left(L))                                                            !EQN 159
                    FNO3 = 1.0-EXP(TVR1*NO3CF * NO3Left(L))                                                            !EQN 160
                    IF (NO3LEFT(L) <= NO3MN) FNO3 = 0.0  
                    IF (FNO3 > 1.0)  FNO3 = 1.0
                    IF (NH4LEFT(L) <= NH4MN) FNH4 = 0.0  
                    IF (FNH4 > 1.0)  FNH4 = 1.0
                    ! Water effects
                    IF (SW(L) <= DUL(L)) THEN
                        WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L))                                                      !EQN 161 
                    ELSE
                        WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                        WFNU = 1.0 ! Wet soil effect not implemented
                    ENDIF
                    IF (WFNU < 0.0) WFNU = 0.0
                    ! LAH Note that WFNU squared
                    TVR2 = AMAX1(0.0,1.0 - H2OCF*(1.0-(WFNU*WFNU)))
                    RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0                                                            !EQN 162
                    RNO3U(L) = RFAC * FNO3 * RTNO3                                                                     !EQN 189
                    RNH4U(L) = RFAC * FNH4 * RTNH4                                                                     !EQN 190
                    RNO3U(L) = MAX(0.0,RNO3U(L))
                    RNH4U(L) = MAX(0.0,RNH4U(L))
                    NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha                                                      !EQN 191
                ENDIF
            ENDDO
    
            ! Ratio (NUPRATIO) to indicate N supply for output
            IF (ANDEM > 0.0) THEN
                NUPRATIO = NUPAP/ANDEM                                                                                 !EQN 192
            ELSE
                IF (NUPAP > 0.0) THEN
                    NUPRATIO = 10.0
                ELSE  
                    NUPRATIO = 0.0
                ENDIF  
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            NUF = 1.0
            IF (NUPAP > 0.0) THEN
                NUF = AMIN1(1.0,ANDEM/NUPAP)                                                                           !EQN 193
            ENDIF 
    
            ! Actual N uptake by layer roots based on demand (kg/ha)
            UNO3 = 0.0
            UNH4 = 0.0
            NUPD = 0.0
            NUPAD = 0.0
            DO L = 1, NLAYRROOT
                UNO3(L) = RNO3U(L)*NUF                                                                                 !EQN 196
                UNH4(L) = RNH4U(L)*NUF                                                                                 !EQN 198
                IF (FAC(L) <= 0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NO3MN/FAC(L)                                                                                !EQN 194
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))                                                          !EQN 197
                IF (FAC(L) <= 0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NH4MN/FAC(L)                                                                                !EQN 195 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))                                                          !EQN 199
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                                                                      !EQN 200              
            END DO
            IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(plantPopulation())                                                                             !EQN 201
            ELSE
                NUPD = 0.
            ENDIF
    
            SEEDNUSE2 = 0.0
            ! Seed use after using reserves and uptake
            IF (RTWT > 0.0 .AND. ISWNIT /= 'N') THEN
                SEEDNUSE2 = AMAX1(0.0,AMIN1(SEEDN-SEEDNUSE,LNDEM+SNDEM+RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))  !EQN 205
            ELSE
                SEEDNUSE2 = 0.0
            ENDIF
    
            ! Distribute N to leaves,stem,root,and storage root
            LNUSE = 0.0
            SNUSE = 0.0
            RNUSE = 0.0
            SRNUSE = 0.0
            SNUSEN = 0.0                                                                              !LPM23MAY2015 To consider different N use by node according with age
            NULEFT = SEEDNUSE+SEEDNUSE2+RSNUSED+NUPD                                                                   !EQN 206
            node%NDEMSMN = 0.0      !LPM14SEP2017 Initialize the variable with 0
    
            ! For supplying minimum
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    !NDEMSMN(BR,LF) = ((GROST+GROCR)/(GROSTP+GROCR))*NODEWTG(BR,LF)*SNCM(BR,LF) !LMP 02SEP2016 To consider potential growth
                    node(BR,LF)%NDEMSMN = node(BR,LF)%NODEWTG * node(BR,LF)%SNCM
                ENDDO
            ENDDO
            !NDEMMN = GROLF*LNCM+RTWTG*RNCM+(GROST+GROCR)*SNCM+GROSR*(SRNPCS/100.0)*0.5                                 !EQN 207 !LPM 25MAY2015 To consider different N concentration by node according with node age 
            !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
            !NDEMMN = GROLF*LNCM+RTWTG*RNCM+SUM(NDEMSMN)  !LPM 24APR2016 using GROLFP instead of GROLF
            !LNUSE(1) = (GROLF*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                           !EQN 208
            NDEMMN = GROLFP*LNCM+RTWTG*RNCM+SUM(node%NDEMSMN) 
            LNUSE(1) = (GROLFP*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                           !EQN 208
            RNUSE(1) = (RTWTG*RNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                           !EQN 209
            !SNUSE(1) = ((GROST+GROCR)*SNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                   !EQN 210
            
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (GROSTP > 0.0) THEN
                        !SNUSEN(1,BR,LF) = ((GROST+GROCR)/(GROSTP+GROCR))*NODEWTG(BR,LF)*SNCM(BR,LF)* & !LPM 02SEP2016 To use potential growth 
                        SNUSEN(1,BR,LF) = node(BR,LF)%NODEWTG * node(BR,LF)%SNCM * AMIN1(1.0,NULEFT/NDEMMN)
                        SNUSE(1) = SNUSE(1)+ SNUSEN(1,BR,LF)
                    ENDIF
                ENDDO
            ENDDO
            !SRNUSE(1) = (GROSR*(SRNPCS/100.0)*0.5)*AMIN1(1.0,NULEFT/NDEMMN)                                            !EQN 211 !LPM 05JUN2105 GROSR or basic growth of storage roots will not be used
    
            ! Reduce stem,Plant. stick,root growth if N < supply minimum
            IF (NDEMMN > NULEFT) THEN
                GROSTADJ = GROSTP*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 213
                GROCRADJ = GROCRP*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 214
                RTWTGADJ = RTWTG*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 215
                RTRESPADJ = RTWTGADJ*RRESP/(1.0-RRESP)                                                                 !EQN 216   
            ELSE
                !GROSTADJ = GROST   !LPM 02SEP2016 Use potential growth
                !GROCRADJ = GROCR
                GROSTADJ = GROSTP
                GROCRADJ = GROCRP
                RTWTGADJ = RTWTG
                RTRESPADJ = RTRESP   
            ENDIF
    
            !NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)-SRNUSE(1)                                                     !EQN 212 !LPM 05JUN2105 SRNUSE(1) for basic growth of storage roots will not be used
            NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)
            ! 5.For leaf growth to standard N (N to leaves first)
            !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX)-LNUSE(1))                                                             !EQN 217 !LPM 02SEP2016 To use potential growth instead of CHO restricted growth
            LNUSE(2) = AMIN1(NULEFT,(GROLFP*LNCX)-LNUSE(1))                                                             !EQN 217
            !Could use the NLLG parameter but may need to adjust 
            !the photosynthesis-leaf N response parameters, or
            !the standard PARUE  
            !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX*NLLG)-LNUSE(1))
            NULEFT = NULEFT - LNUSE(2)                                                                                 !EQN 218
    
            ! 6.For distribution of remaining N to st,rt,storage root
            !NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM-SRNUSE(1)                                                     !EQN 219 !LPM 05JUN2105 SRNUSE(1) for basic growth of storage roots will not be used
            NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM                                                                !EQN 219
            IF (NDEM2 > 0.0)THEN
                !SNUSE(2) = (SNDEM-SNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)                                                  !EQN 220
                DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                    DO LF = 1, LNUMSIMSTG(BR)
                        IF (GROSTP > 0.0) THEN
                            SNUSEN(2,BR,LF) = (node(BR,LF)%SNDEMN - SNUSEN(1,BR,LF))* AMIN1(1.0,NULEFT/NDEM2)
                            SNUSE(2) = SNUSE(2)+ SNUSEN(2,BR,LF)
                        ENDIF
                    ENDDO
                ENDDO
                RNUSE(2) = (RNDEM-RNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)                                                  !EQN 221
                SRNUSE(2) = (SRNDEM)*AMIN1(1.0,NULEFT/NDEM2)                                                           !EQN 222
                NULEFT = NULEFT - SNUSE(2) - RNUSE(2) - SRNUSE(2)                                                      !EQN 223
                IF (NULEFT > 0.0) THEN
                    LNUSE(3) = NULEFT                                                                                  !EQN 224
                ELSE
                    LNUSE(3) = 0.0
                ENDIF
            ELSE
                LNUSE(3) = 0.0
                SNUSE(2) = 0.0
                RNUSE(2) = 0.0
                SRNUSE(2) = 0.0
            ENDIF  
    
            LNUSE(0) = LNUSE(1) + LNUSE(2) + LNUSE(3)                                                                  !EQN 225
            SNUSE(0) = SNUSE(1) + SNUSE(2)                                                                             !EQN 226
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (GROSTP > 0.0) THEN
                        SNUSEN(0,BR,LF) = SNUSEN(1,BR,LF) + SNUSEN(2,BR,LF)
                    ENDIF
                ENDDO
            ENDDO
            RNUSE(0) = RNUSE(1) + RNUSE(2)                                                                             !EQN 227 
            !SRNUSE(0) = SRNUSE(1) + SRNUSE(2)                                                                          !EQN 228 !LPM 05JUN2105 SRNUSE(1) for basic growth of storage roots will not be used
            SRNUSE(0) = SRNUSE(2)                                                                                      !EQN 228    
            
            ! N Pools available for re-mobilization
            NUSEFAC = NLABPC/100.0                                                                                     !EQN 229
            NPOOLR = AMAX1 (0.0,((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))                                                   !EQN 230
            NPOOLL = AMAX1 (0.0,((LFWT-leafTotalSenescedWeight())*(LANC-LNCM)*NUSEFAC))                                          !EQN 231
            !NPOOLS = AMAX1 (0.0,((woodyWeight())*(SANC-SNCM)*NUSEFAC))                                                     !EQN 232
            NPOOLS = 0
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)          
                    IF(STWTP+CRWTP > 0.0)THEN
                        node(BR,LF)%NPOOLSN = AMAX1 (0.0,((node(BR,LF)%NODEWT * (woodyWeight())/(STWTP+CRWTP))*( node(BR,LF)%SANC - node(BR,LF)%SNCM )*NUSEFAC))  
                    ELSE
                        node(BR,LF)%NPOOLSN = 0.0
                    ENDIF
                    NPOOLS =  NPOOLS + node(BR,LF)%NPOOLSN                                                                 !EQN 232
                ENDDO
            ENDDO
            ! Check N and reduce leaf growth if not enough N  
            IF (ABS(NULEFT) <= 1.0E-5) THEN   ! Inadequate N
                IF (NLLG > 0.0 .AND. LNCX > 0.0 .AND. GROLFP > 0.0) THEN 
                    !IF ((LNUSE(1)+LNUSE(2))/GROLF < (LNCX*NLLG)) THEN  !LPM 02SEP2016 Use GROLFP instead of GROLF
                    IF ((LNUSE(1)+LNUSE(2))/GROLFP < (LNCX*NLLG)) THEN 
                        GROLFADJ = (LNUSE(1)+LNUSE(2))/(LNCX*NLLG)                                                     !EQN 233a
                    ELSE  
                        !GROLFADJ = GROLF                                                                               !EQN 233b !LPM 02SEP2016 Use GROLFP instead of GROLF
                        GROLFADJ = GROLFP                                                                               !EQN 233b
                    ENDIF  
                ENDIF
                !RSSRWTGLFADJ = GROLF - GROLFADJ                                                                        !EQN 234 !LPM 02SEP2016 Keep GROLF but restrict to 0 if N growth is greater than CHO growth (GROLF)
                RSSRWTGLFADJ = AMAX1(0.0,GROLF - GROLFADJ)                                                              !EQN 234 
                !AREAPOSSIBLEN =GROLFADJ*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))                                              !EQN 235 !LPM 12DEC2016 Delete temperature, water and leaf position factors in SLA 
                 AREAPOSSIBLEN =GROLFADJ*(1.0-LPEFR)*LAWL(1)                                              !EQN 235 
                
                ! If not enough N set N factor
                !IF (PLAGSB3 > AREAPOSSIBLEN.AND.PLAGSB3 > 0.0)THEN !LPM 02SEP2016 Use of PLAGSB2 instead of PLAGSB3
                IF (PLAGSB2 > AREAPOSSIBLEN .AND. PLAGSB2 > 0.0)THEN
                        node(0,0)%NFLF2 = AREAPOSSIBLEN/PLAGSB2                                                                   !EQN 236
                ELSE    
                        node(0,0)%NFLF2 = 1.0
                ENDIF 
                
        
            ELSE   ! Adequate N 
        
                !NFLF2(0) = 1.0                                                                                           !LPM 21MAR15
                !DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1                                                      !LPM 21MAR15 Change to include cohorts BR,L
                node(0,0)%NFLF2 = 1.0

                
            ENDIF
    
    
        ENDIF    ! End of N uptake and growth adjustmenets
    
    ! Area and assimilate factors for each leaf
                DO BR = 0, BRSTAGE                                                                                        !LPM 23MAR15 To consider cohorts
                    DO LF = 1, LNUMSIMSTG(BR)   
                        IF (node(BR,LF)%LAGETT <= LLIFGTT .AND. node(BR,LF)%LAPOTX2 > 0.0 ) THEN
                            IF (LNUMSIMSTG(BR) < LCNUMX) THEN
                                !LPM 15NOV15 Variables LAGL3, LAGL3T and LATL3T created to save the actual leaf are by cohort (all the plant (all branches and shoots))
                                !LAGL3(BR,LF) = LAGL(BR,LF) * AMIN1(AFLF(0,0),WFG,NFG) !LPM 02SEP2016  Use of NFLF2 instead of NFG
                                node(BR,LF)%LAGL3 = node(BR,LF)%LAGL * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2) 
                                !LATL3(BR,LF)= LATL2(BR,LF)-LAGL(BR,LF) + LAGL3(BR,LF)                                             !EQN 150 !LPM  15NOV15 The reduction is just in the leaf area growing
                                node(BR,LF)%LATL3= node(BR,LF)%LATL3 + node(BR,LF)%LAGL3                                             !EQN 150 !LPM 24APR2016 to keep leaf area value with stress
                                node(BR,LF)%AFLF = AMIN1(1.0,node(BR,LF)%AFLF + AMAX1(0.0,node(0,0)%AFLF) * (node(BR,LF)%LAGL)/node(BR,LF)%LAPOTX2)             !EQN 151   
                                node(BR,LF)%NFLF2 = AMIN1(1.0,node(BR,LF)%NFLF2 + AMAX1(0.0,node(0,0)%NFLF2) * (node(BR,LF)%LAGL)/node(BR,LF)%LAPOTX2)  !EQN 237 !LPM 02SEP2016 To save NFLF2
                                node(BR,LF)%LAGL3T = node(BR,LF)%LAGL3*BRNUMST(BR) 
                                node(BR,LF)%LATL3T = node(BR,LF)%LATL3*BRNUMST(BR)
                                DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                                    IF (SHNUM-FLOAT(L-1) > 0.0) THEN
                                        node(BR,LF)%LAGL3T = node(BR,LF)%LAGL3T+(node(BR,LF)%LAGL3*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
                                        node(BR,LF)%LATL3T = node(BR,LF)%LATL3T+(node(BR,LF)%LATL3*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                                    ENDIF
                                ENDDO
                                !IF (CFLAFLF == 'N') AFLF(BR,LF) = 1.0                                                 !LPM 23MAR15 Define previously  
                                IF (LF == LNUMSIMSTG(BR) .AND. LNUMG > LNUMNEED .AND. BR == BRSTAGE) THEN                                             ! This is where new leaf is initiated
                                    node(BR,LF+1)%LAGL3 = node(BR,LF+1)%LAGL * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2)
                                    node(BR,LF+1)%LATL3= node(BR,LF+1)%LATL3 + node(BR,LF+1)%LAGL3                                             !EQN 150 !LPM 24APR2016 to keep leaf area value with stress
                                    node(BR,LF+1)%AFLF = AMIN1(1.0,node(BR,LF+1)%AFLF + AMAX1(0.0,node(0,0)%AFLF) * (node(BR,LF+1)%LAGL)/node(BR,LF+1)%LAPOTX2)             !EQN 151   
                                    node(BR,LF+1)%NFLF2 = AMIN1(1.0,node(BR,LF+1)%NFLF2 + AMAX1(0.0,node(0,0)%NFLF2) * (node(BR,LF)%LAGL)/node(BR,LF+1)%LAPOTX2)  !EQN 237 !LPM 02SEP2016 To save NFLF2
                                    node(BR,LF+1)%LAGL3T = node(BR,LF+1)%LAGL3*BRNUMST(BR) 
                                    node(BR,LF+1)%LATL3T = node(BR,LF+1)%LATL3*BRNUMST(BR)
                                    DO L = 2,INT(SHNUM+2) ! L is shoot cohort,main=cohort 1
                                        IF (SHNUM-FLOAT(L-1) > 0.0) THEN
                                            node(BR,LF+1)%LAGL3T = node(BR,LF+1)%LAGL3T+(node(BR,LF+1)%LAGL3*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))                  
                                            node(BR,LF+1)%LATL3T = node(BR,LF+1)%LATL3T+(node(BR,LF+1)%LATL3*BRNUMST(BR))*SHGR(L) * AMAX1(0.,AMIN1(FLOAT(L),SHNUM)-FLOAT(L-1))  
                                        ENDIF
                                    ENDDO
                                ENDIF
                            ENDIF  
                         ELSE
                            IF (node(BR,LF)%LAPOTX2 <= 0.0 ) THEN
                                node(BR,LF)%AFLF = 1.0             !EQN 151   
                                node(BR,LF)%NFLF2 = 1.0
                            ENDIF
                        ENDIF
                    ENDDO
                ENDDO

            PLAGSB4 = PLAGSB2 * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2) 
            SHLAGB4(1) = SHLAG2(1) * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2)                                                                          !EQN 240
            SHLAGB4(2) = SHLAG2(2) * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2)                                                                          !EQN 240
            SHLAGB4(3) = SHLAG2(3) * AMIN1(node(0,0)%AFLF,WFG,node(0,0)%NFLF2)                                                                         !EQN 240    

            GROCRADJ = AMIN1(GROCR,GROCRADJ)
            !GROLFADJ = AMIN1(GROLF,GROLFADJ)
                        ! Potential leaf weight increase.
            !LPM 16DEC2016 GROLFADJ is defined by the new estimation of leaf area which is considering water stress (WFG), carbohydrates available (AFLF) and nitrogen restrictions (NFLF2)
            IF (LAWL(1) > 0.0) GROLFADJ = (PLAGSB4/LAWL(1)) / (1.0-LPEFR)                                                   !EQN 297    
            !LPM 09OCT2019 Adding the WFG at the same time than the restrictions due to N and assimilates (selecting the most limiting)
            ! No water stress factor added to the roots
            GROSTADJ = AMIN1(GROST,GROSTADJ, (GROSTP * WFG))
            RSSRWTGLFADJ = AMAX1(0.0, RSSRWTGLFADJ)
            RTRESPADJ = AMIN1(RTRESP,RTRESPADJ)    
            RTWTGADJ = AMIN1(RTWTG,RTWTGADJ)
    END SUBROUTINE YCA_Growth_NUptake