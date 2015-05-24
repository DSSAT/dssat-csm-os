!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 5047 - 5350 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module CS_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_NUptake calculates N uptake and distribution and adjusts growth if N inadequate.
!***************************************************************************************************************************

    SUBROUTINE CS_Growth_NUptake ( &
        BD          , DLAYR       , DUL         , ISWNIT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , &
        RLV         , SAT         , SW          , UNH4        , UNO3        , BRSTAGE     & 
        )
    
        USE ModuleDefs
        USE CS_First_Trans_m
    
        IMPLICIT NONE
        
    
        CHARACTER(LEN=1) ISWNIT      
        INTEGER NLAYR       
        REAL    BD(NL)      , DLAYR(NL)   , DUL(NL)     , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , RLV(NL)     , SAT(NL)     
        REAL    SW(NL)      , UNH4(NL)    , UNO3(NL)    , BRSTAGE
    
        
        !-----------------------------------------------------------------------
        !           N Uptake  -  and growth adjustments if N inadequate
        !-----------------------------------------------------------------------

        ! Set adjusted values to unadjusted 
        ! For when no adjustment necessary, or when not simulating N
        GROCRADJ = GROCR
        GROLFADJ = GROLF
        GROSTADJ = GROST
        PLAGSB4 = PLAGSB3
        RSSRWTGLFADJ = 0.0
        RTRESPADJ = RTRESP   
        RTWTGADJ = RTWTG
        SHLAGB4 = SHLAGB3

        IF (ISWNIT.NE.'N') THEN
    
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
                FAC(L) = 10.0/(BD(L)*DLAYR(L))                                                                         !EQN 169
                SNO3(L) = NO3LEFT(L) / FAC(L)                                                                          !EQN 170
                SNH4(L) = NH4LEFT(L) / FAC(L)                                                                          !EQN 186
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L).GT.0.0) THEN
                    SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                    SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
            END DO
    
            LNDEM = GROLF*LNCX + (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) - GROLSRTN                        !EQN 152
            !SNDEM = AMAX1(0.0,GROST+GROCR)*SNCX + (STWT+CRWT)*AMAX1(0.0,NTUPF*(SNCX-SANC))                             !EQN 153
            RNDEM = RTWTG*RNCX + (RTWT-SENRTG-GROLSRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))                                    !EQN 154
            SRNDEM = (GROSR+SRWTGRS)*(SRNPCS/100.0) + SRWT*AMAX1(0.0,NTUPF*((SRNPCS/100.0)-SRANC))                     !EQN 155
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N demand by node according with its age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (GROSTP.GT.0.0) THEN
                        SNDEMN(BR,LF) = AMAX1(0.0,((GROST+GROCR)/(GROSTP+GROCR))*NODEWTG(BR,LF))*SNCX(BR,LF) + &
                              (NODEWT(BR,LF)*(STWT+CRWT)/(STWTP+CRWTP))*AMAX1(0.0,NTUPF*(SNCX(BR,LF)-SANC(BR,LF)))  
                        SNDEM = SNDEM + SNDEMN(BR,LF)
                    ENDIF
                ENDDO
            ENDDO
            ! Seed use if no roots
            ! N use same % of initial as for CH20,if needed.
            IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0, AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))                          !EQN 203a
            ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN, 0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))                    !EQN 203b
            ENDIF
    
            ! Reserves used before uptake
            RSNUSED = AMIN1(LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE,RSN)                                                     !EQN 156
    
            ! N uptake needed 
            ANDEM = AMAX1(0.0,PLTPOP*10.0* (LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSED))                                !EQN 158
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
                IF (RLV(L) .GT. 0.0) THEN
                    NLAYRROOT = L
                    ! N concentration effects
                    FNH4 = 1.0-EXP(TVR1*NH4CF * NH4LEFT(L))                                                            !EQN 159
                    FNO3 = 1.0-EXP(TVR1*NO3CF * NO3LEFT(L))                                                            !EQN 160
                    IF (NO3LEFT(L) .LE. NO3MN) FNO3 = 0.0  
                    IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                    IF (NH4LEFT(L) .LE. NH4MN) FNH4 = 0.0  
                    IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                    ! Water effects
                    IF (SW(L) .LE. DUL(L)) THEN
                        WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L))                                                      !EQN 161 
                    ELSE
                        WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                        WFNU = 1.0 ! Wet soil effect not implemented
                    ENDIF
                    IF (WFNU.LT.0.0) WFNU = 0.0
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
            IF (ANDEM.GT.0) THEN
                NUPRATIO = NUPAP/ANDEM                                                                                 !EQN 192
            ELSE
                IF (NUPAP.GT.0.0) THEN
                    NUPRATIO = 10.0
                ELSE  
                    NUPRATIO = 0.0
                ENDIF  
            ENDIF
            ! Factor (NUF) to reduce N uptake to level of demand
            NUF = 1.0
            IF (NUPAP.GT.0.0) THEN
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
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NO3MN/FAC(L)                                                                                !EQN 194
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))                                                          !EQN 197
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NH4MN/FAC(L)                                                                                !EQN 195 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))                                                          !EQN 199
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                                                                      !EQN 200              
            END DO
            IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)                                                                             !EQN 201
            ELSE
                NUPD = 0.
            ENDIF
    
            SEEDNUSE2 = 0.0
            ! Seed use after using reserves and uptake
            IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
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
    
            ! For supplying minimum
            NDEMMN = GROLF*LNCM+RTWTG*RNCM+(GROST+GROCR)*SNCM+GROSR*(SRNPCS/100.0)*0.5                                 !EQN 207
            LNUSE(1) = (GROLF*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                           !EQN 208
            RNUSE(1) = (RTWTG*RNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                           !EQN 209
            !SNUSE(1) = ((GROST+GROCR)*SNCM)*AMIN1(1.0,NULEFT/NDEMMN)                                                   !EQN 210
            
            DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                DO LF = 1, LNUMSIMSTG(BR)
                    IF (GROSTP.GT.0.0) THEN
                        SNUSEN(1,BR,LF) = ((GROST+GROCR)/(GROSTP+GROCR))*NODEWTG(BR,LF)*SNCM(BR,LF))*  &
                                   AMIN1(1.0,NULEFT/NDEMMN)
                        SNUSE(1) = SNUSE(1)+ SNUSEN(1,BR,LF)
                    ENDIF
                ENDDO
            ENDDO
            SRNUSE(1) = (GROSR*(SRNPCS/100.0)*0.5)*AMIN1(1.0,NULEFT/NDEMMN)                                            !EQN 211
    
            ! Reduce stem,Plant. stick,root growth if N < supply minimum
            IF (NDEMMN.GT.NULEFT) THEN
                GROSTADJ = GROST*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 213
                GROCRADJ = GROCR*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 214
                RTWTGADJ = RTWTG*AMIN1(1.0,NULEFT/NDEMMN)                                                              !EQN 215
                RTRESPADJ = RTWTGADJ*RRESP/(1.0-RRESP)                                                                 !EQN 216   
            ELSE
                GROSTADJ = GROST
                GROCRADJ = GROCR
                RTWTGADJ = RTWTG
                RTRESPADJ = RTRESP   
            ENDIF
    
            NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)-SRNUSE(1)                                                     !EQN 212
    
            ! 5.For leaf growth to standard N (N to leaves first)
            LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX)-LNUSE(1))                                                             !EQN 217
            !Could use the NLLG parameter but may need to adjust 
            !the photosynthesis-leaf N response parameters, or
            !the standard PARUE  
            !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX*NLLG)-LNUSE(1))
            NULEFT = NULEFT - LNUSE(2)                                                                                 !EQN 218
    
            ! 6.For distribution of remaining N to st,rt,storage root
            NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM-SRNUSE(1)                                                     !EQN 219
            IF (NDEM2.GT.0.0)THEN
                !SNUSE(2) = (SNDEM-SNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)                                                  !EQN 220
                DO BR = 0, BRSTAGE                                                                                        !LPM23MAY2015 To consider different N concentration by node according with age                                                                       
                    DO LF = 1, LNUMSIMSTG(BR)
                        IF (GROSTP.GT.0.0) THEN
                            SNUSEN(2,BR,LF) = (SNDEMN(BR,LF)-SNUSEN(1,BR,LF))* AMIN1(1.0,NULEFT/NDEM2)
                            SNUSE(2) = SNUSE(2)+ SNUSEN(2,BR,LF)
                        ENDIF
                    ENDDO
                ENDDO
                RNUSE(2) = (RNDEM-RNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)                                                  !EQN 221
                SRNUSE(2) = (SRNDEM-SRNUSE(1))*AMIN1(1.0,NULEFT/NDEM2)                                                 !EQN 222
                NULEFT = NULEFT - SNUSE(2) - RNUSE(2) - SRNUSE(2)                                                      !EQN 223
                IF (NULEFT.GT.0.0) THEN
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
                    IF (GROSTP.GT.0.0) THEN
                        SNUSEN(0,BR,LF) = SNUSEN(1,BR,LF) + SNUSEN(2,BR,LF)
                    ENDIF
                ENDDO
            ENDDO
            RNUSE(0) = RNUSE(1) + RNUSE(2)                                                                             !EQN 227 
            SRNUSE(0) = SRNUSE(1) + SRNUSE(2)                                                                          !EQN 228 
    
            ! N Pools available for re-mobilization
            NUSEFAC = NLABPC/100.0                                                                                     !EQN 229
            NPOOLR = AMAX1 (0.0,((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))                                                   !EQN 230
            NPOOLL = AMAX1 (0.0,((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))                                          !EQN 231
            NPOOLS = AMAX1 (0.0,((STWT+CRWT)*(SANC-SNCM)*NUSEFAC))                                                     !EQN 232
    
            ! Check N and reduce leaf growth if not enough N  
            IF (ABS(NULEFT).LE.1.0E-5) THEN   ! Inadequate N
                IF (NLLG.GT.0.0.AND.LNCX.GT.0.0) THEN 
                    IF ((LNUSE(1)+LNUSE(2))/GROLF.LT.(LNCX*NLLG)) THEN 
                        GROLFADJ = (LNUSE(1)+LNUSE(2))/(LNCX*NLLG)                                                     !EQN 233a
                    ELSE  
                        GROLFADJ = GROLF                                                                               !EQN 233b
                    ENDIF  
                ENDIF
                RSSRWTGLFADJ = GROLF - GROLFADJ                                                                        !EQN 234
        
                AREAPOSSIBLEN =GROLFADJ*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))                                              !EQN 235
        
                !! If not enough N set N factor
                !IF (PLAGSB3.GT.AREAPOSSIBLEN.AND.PLAGSB3.GT.0.0)THEN
                !    NFLF2(0) = AREAPOSSIBLEN/PLAGSB3                                                                   !EQN 236
                !ELSE  
                !    NFLF2(0) = 1.0
                !ENDIF
        
                ! Area and assimilate factors for each leaf
                !DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1                                                      !LPM 21MAR15 Change to include cohorts BR,L
                !     IF (LNUMSG.LT.LNUMX) THEN
                !        LATL4(L)= LATL3(L) * NFLF2(0)                                                                           !EQN 241            
                !        NFLF2(L) = AMIN1(1.0,NFLF2(L) + AMAX1(0.0,NFLF2(0)) * (LATLPOT(L)-LATLPREV(L))/LAPOTX(L))               !EQN 237
                !     ENDIF
                !ENDDO    
                
                ! If not enough N set N factor
                IF (PLAGSB3.GT.AREAPOSSIBLEN.AND.PLAGSB3.GT.0.0)THEN
                        NFLF2(0,0) = AREAPOSSIBLEN/PLAGSB3                                                                   !EQN 236
                ELSE  
                        NFLF2(0,0) = 1.0
                ENDIF 
                
               ! Area and assimilate factors for each leaf
                DO BR = 0, BRSTAGE                                                                                        !LPM 21MAR15
                    ! If not enough N set N factor
                    DO LF = 1, LNUMSIMSTG(BR)                                                                              !LPM 21MAR15
                        IF (LNUMSIMSTG(BR).LT.LCNUMX) THEN
                            LATL4(BR,LF)= LATL3(BR,LF) * NFLF2(0,0)                                                                           !EQN 241            
                            NFLF2(BR,LF) = AMIN1(1.0,NFLF2(BR,LF) + AMAX1(0.0,NFLF2(0,0)) * (LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))      !EQN 237
                        ENDIF
                    ENDDO
                ENDDO
                PLAGSB4 = PLAGSB3 * NFLF2(0,0)                                                                           !EQN 238
                SHLAGB4(1) = SHLAGB3(1) * NFLF2(0,0)                                                                      !EQN 239
                SHLAGB4(2) = SHLAGB3(2) * NFLF2(0,0)
                SHLAGB4(3) = SHLAGB3(3) * NFLF2(0,0)
                !PLAGSB4 = PLAGSB3 * NFLF2(0)                                                                           !EQN 238
                !SHLAGB4(1) = SHLAGB3(1) * NFLF2(0)                                                                     !EQN 239
                !SHLAGB4(2) = SHLAGB3(2) * NFLF2(0)
                !SHLAGB4(3) = SHLAGB3(3) * NFLF2(0)
        
            ELSE   ! Adequate N 
        
                !NFLF2(0) = 1.0                                                                                           !LPM 21MAR15
                !DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1                                                      !LPM 21MAR15 Change to include cohorts BR,L
                NFLF2(0,0) = 1.0
                DO BR = 0, BRSTAGE                                                                                        !LPM 21MAR15
                    DO LF = 1, LNUMSIMSTG(BR)                                                                              !LPM 21MAR15
                        IF (LNUMSIMSTG(BR).LT.LCNUMX) THEN
                            LATL4(BR,LF)= LATL3(BR,LF) * NFLF2(0,0)            
                            NFLF2(BR,LF) = AMIN1(1.0,NFLF2(BR,LF) + AMAX1(0.0,NFLF2(0,0)) * (LATLPOT(BR,LF)-LATLPREV(BR,LF))/LAPOTX(BR,LF))
                        ENDIF
                    ENDDO
                ENDDO
                
            ENDIF
    
        ELSE     ! ISWNIT = N   
    
            LATL4 = LATL3
            NFLF2 = 1.0            
    
        ENDIF    ! End of N uptake and growth adjustmenets
    END SUBROUTINE CS_Growth_NUptake