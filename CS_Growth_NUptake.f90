!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.RATE) lines 5034 - 5337 of the original CSCAS code.The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Growth_NUptake calculates N uptake and distribution and adjusts growth if N inadequate.
!***************************************************************************************************************************

    SUBROUTINE CS_Growth_NUptake ( &
        BD          , DLAYR       , DUL         , ISWNIT      , LL          , NH4LEFT     , NLAYR       , NO3LEFT     , &
        RLV         , SAT         , SW          , UNH4        , UNO3          & 
        )
    
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
    
        IMPLICIT NONE
        
    
        CHARACTER(LEN=1) ISWNIT      
        INTEGER NLAYR       
        REAL    BD(NL)      , DLAYR(NL)   , DUL(NL)     , LL(NL)      , NH4LEFT(NL) , NO3LEFT(NL) , RLV(NL)     , SAT(NL)     
        REAL    SW(NL)      , UNH4(NL)    , UNO3(NL)
    
        
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
                FAC(L) = 10.0/(BD(L)*DLAYR(L))
                SNO3(L) = NO3LEFT(L) / FAC(L)
                SNH4(L) = NH4LEFT(L) / FAC(L)
                SNO3PROFILE = SNO3PROFILE + SNO3(L)
                SNH4PROFILE = SNH4PROFILE + SNH4(L)
                IF (RLV(L).GT.0.0) THEN
                    SNO3ROOTZONE = SNO3ROOTZONE + SNO3(L)
                    SNH4ROOTZONE = SNH4ROOTZONE + SNH4(L)
                ENDIF
            END DO
    
            LNDEM = GROLF*LNCX + (LFWT-SENLFG-SENLFGRS)*AMAX1(0.0,NTUPF*(LNCX-LANC)) - GROLSRTN
            SNDEM = AMAX1(0.0,GROST+GROCR)*SNCX + (STWT+CRWT)*AMAX1(0.0,NTUPF*(SNCX-SANC))
            RNDEM = RTWTG*RNCX + (RTWT-SENRTG-GROLSRT)*AMAX1(0.0,NTUPF*(RNCX-RANC))
            SRNDEM = (GROSR+SRWTGRS)*(SRNPCS/100.0) + SRWT*AMAX1(0.0,NTUPF*((SRNPCS/100.0)-SRANC)) 
    
    
            ! Seed use if no roots
            ! N use same % of initial as for CH20,if needed.
            IF (RTWT.LE.0.0) THEN
                SEEDNUSE = AMAX1(0.0, AMIN1(SEEDN,LNDEM+SNDEM+RNDEM,SEEDNI/SDDUR*(TT/STDAY)))
            ELSE
                ! Some use of seed (0.5 need) even if may not be needed
                SEEDNUSE = AMAX1(0.0,AMIN1(SEEDN, 0.5*(LNDEM+SNDEM+RNDEM),SEEDNI/SDDUR*(TT/STDAY)))
            ENDIF
    
            ! Reserves used before uptake
            RSNUSED = AMIN1(LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE,RSN)
    
            ! N uptake needed 
            ANDEM = AMAX1(0.0,PLTPOP*10.0* (LNDEM+SNDEM+RNDEM+SRNDEM-SEEDNUSE-RSNUSED))
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
                    FNH4 = 1.0-EXP(TVR1*NH4CF * NH4LEFT(L))
                    FNO3 = 1.0-EXP(TVR1*NO3CF * NO3LEFT(L))
                    IF (NO3LEFT(L) .LE. NO3MN) FNO3 = 0.0  
                    IF (FNO3 .GT. 1.0)  FNO3 = 1.0
                    IF (NH4LEFT(L) .LE. NH4MN) FNH4 = 0.0  
                    IF (FNH4 .GT. 1.0)  FNH4 = 1.0
                    ! Water effects
                    IF (SW(L) .LE. DUL(L)) THEN
                        WFNU = (SW(L) - LL(L)) / (DUL(L) - LL(L)) 
                    ELSE
                        WFNU = 1.0-(SW(L)-DUL(L))/(SAT(L)-DUL(L))
                        WFNU = 1.0 ! Wet soil effect not implemented
                    ENDIF
                    IF (WFNU.LT.0.0) WFNU = 0.0
                    ! LAH Note that WFNU squared
                    TVR2 = AMAX1(0.0,1.0 - H2OCF*(1.0-(WFNU*WFNU)))
                    RFAC = RLV(L) * TVR2 * DLAYR(L) * 100.0
                    RNO3U(L) = RFAC * FNO3 * RTNO3
                    RNH4U(L) = RFAC * FNH4 * RTNH4
                    RNO3U(L) = MAX(0.0,RNO3U(L))
                    RNH4U(L) = MAX(0.0,RNH4U(L))
                    NUPAP = NUPAP + RNO3U(L) + RNH4U(L) !kg[N]/ha
                ENDIF
            ENDDO
    
            ! Ratio (NUPRATIO) to indicate N supply for output
            IF (ANDEM.GT.0) THEN
                NUPRATIO = NUPAP/ANDEM
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
                NUF = AMIN1(1.0,ANDEM/NUPAP)
            ENDIF 
    
            ! Actual N uptake by layer roots based on demand (kg/ha)
            UNO3 = 0.0
            UNH4 = 0.0
            NUPD = 0.0
            NUPAD = 0.0
            DO L = 1, NLAYRROOT
                UNO3(L) = RNO3U(L)*NUF
                UNH4(L) = RNH4U(L)*NUF
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NO3MN/FAC(L) 
                ENDIF  
                UNO3(L) = MAX(0.0,MIN (UNO3(L),SNO3(L)-XMIN))
                IF (FAC(L).LE.0.0) THEN
                    XMIN = 0.0
                ELSE  
                    XMIN = NH4MN/FAC(L) 
                ENDIF  
                XMIN = NH4MN/FAC(L) 
                UNH4(L) = MAX(0.0,MIN (UNH4(L),SNH4(L)-XMIN))
                NUPAD = NUPAD + UNO3(L) + UNH4(L)                  
            END DO
            IF (PLTPOP > 1.E-6) THEN
                NUPD = NUPAD/(PLTPOP*10.0)
            ELSE
                NUPD = 0.
            ENDIF
    
            SEEDNUSE2 = 0.0
            ! Seed use after using reserves and uptake
            IF (RTWT.GT.0.0.AND.ISWNIT.NE.'N') THEN
                SEEDNUSE2 = AMAX1(0.0,AMIN1(SEEDN-SEEDNUSE,LNDEM+SNDEM+RNDEM-RSNUSED-SEEDNUSE-NUPD,SEEDNI/SDDUR*(TT/STDAY)))
            ELSE
                SEEDNUSE2 = 0.0
            ENDIF
    
            ! Distribute N to leaves,stem,root,and storage root
            LNUSE = 0.0
            SNUSE = 0.0
            RNUSE = 0.0
            SRNUSE = 0.0
            NULEFT = SEEDNUSE+SEEDNUSE2+RSNUSED+NUPD
    
            ! For supplying minimum
            NDEMMN = GROLF*LNCM+RTWTG*RNCM+(GROST+GROCR)*SNCM+GROSR*(SRNPCS/100.0)*0.5
            LNUSE(1) = (GROLF*LNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            RNUSE(1) = (RTWTG*RNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            SNUSE(1) = ((GROST+GROCR)*SNCM)*AMIN1(1.0,NULEFT/NDEMMN)
            SRNUSE(1) = (GROSR*(SRNPCS/100.0)*0.5)*AMIN1(1.0,NULEFT/NDEMMN)
    
            ! Reduce stem,crown,root growth if N < supply minimum
            IF (NDEMMN.GT.NULEFT) THEN
                GROSTADJ = GROST*AMIN1(1.0,NULEFT/NDEMMN)
                GROCRADJ = GROCR*AMIN1(1.0,NULEFT/NDEMMN)
                RTWTGADJ = RTWTG*AMIN1(1.0,NULEFT/NDEMMN)
                RTRESPADJ = RTWTGADJ*RRESP/(1.0-RRESP)   
            ELSE
                GROSTADJ = GROST
                GROCRADJ = GROCR
                RTWTGADJ = RTWTG
                RTRESPADJ = RTRESP   
            ENDIF
    
            NULEFT = NULEFT - LNUSE(1)-RNUSE(1)-SNUSE(1)-SRNUSE(1)
    
            ! 5.For leaf growth to standard N (N to leaves first)
            LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX)-LNUSE(1))
            !Could use the NLLG parameter but may need to adjust 
            !the photosynthesis-leaf N response parameters, or
            !the standard PARUE  
            !LNUSE(2) = AMIN1(NULEFT,(GROLF*LNCX*NLLG)-LNUSE(1))
            NULEFT = NULEFT - LNUSE(2)
    
            ! 6.For distribution of remaining N to st,rt,storage root
            NDEM2 = SNDEM-SNUSE(1)+RNDEM-RNUSE(1)+SRNDEM-SRNUSE(1)
            IF (NDEM2.GT.0.0)THEN
                SNUSE(2) = (SNDEM-SNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                RNUSE(2) = (RNDEM-RNUSE(1)) * AMIN1(1.0,NULEFT/NDEM2)
                SRNUSE(2) = (SRNDEM-SRNUSE(1))*AMIN1(1.0,NULEFT/NDEM2)
                NULEFT = NULEFT - SNUSE(2) - RNUSE(2) - SRNUSE(2)
                IF (NULEFT.GT.0.0) THEN
                    LNUSE(3) = NULEFT
                ELSE
                    LNUSE(3) = 0.0
                ENDIF
            ELSE
                LNUSE(3) = 0.0
                SNUSE(2) = 0.0
                RNUSE(2) = 0.0
                SRNUSE(2) = 0.0
            ENDIF  
    
            LNUSE(0) = LNUSE(1) + LNUSE(2) + LNUSE(3)
            SNUSE(0) = SNUSE(1) + SNUSE(2)
            RNUSE(0) = RNUSE(1) + RNUSE(2) 
            SRNUSE(0) = SRNUSE(1) + SRNUSE(2) 
    
            ! N Pools available for re-mobilization
            NUSEFAC = NLABPC/100.0
            NPOOLR = AMAX1 (0.0,((RTWT-SENRTG)*(RANC-RNCM)*NUSEFAC))
            NPOOLL = AMAX1 (0.0,((LFWT-SENLFG-SENLFGRS)*(LANC-LNCM)*NUSEFAC))
            NPOOLS = AMAX1 (0.0,((STWT+CRWT)*(SANC-SNCM)*NUSEFAC))
    
            ! Check N and reduce leaf growth if not enough N  
            IF (ABS(NULEFT).LE.1.0E-5) THEN   ! Inadequate N
                IF (NLLG.GT.0.0.AND.LNCX.GT.0.0) THEN 
                    IF ((LNUSE(1)+LNUSE(2))/GROLF.LT.(LNCX*NLLG)) THEN 
                        GROLFADJ = (LNUSE(1)+LNUSE(2))/(LNCX*NLLG)
                    ELSE  
                        GROLFADJ = GROLF
                    ENDIF  
                ENDIF
                RSSRWTGLFADJ = GROLF - GROLFADJ
        
                AREAPOSSIBLEN =GROLFADJ*(1.0-LPEFR)*(LAWL(1)*(1.0+LAWFF))
        
                ! If not enough N set N factor
                IF (PLAGSB3.GT.AREAPOSSIBLEN.AND.PLAGSB3.GT.0.0)THEN
                    NFLF2(0) = AREAPOSSIBLEN/PLAGSB3
                ELSE  
                    NFLF2(0) = 1.0
                ENDIF
        
                ! Area and assimilate factors for each leaf
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                    IF (LNUMSG.LT.LNUMX) THEN
                        LATL4(L)= LATL3(L) * NFLF2(0)            
                        NFLF2(L) = AMIN1(1.0,NFLF2(L) + AMAX1(0.0,NFLF2(0)) * (LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
                    ENDIF  
                ENDDO
        
                PLAGSB4 = PLAGSB3 * NFLF2(0)
                SHLAGB4(1) = SHLAGB3(1) * NFLF2(0)
                SHLAGB4(2) = SHLAGB3(2) * NFLF2(0)
                SHLAGB4(3) = SHLAGB3(3) * NFLF2(0)
        
            ELSE   ! Adequate N 
        
                NFLF2(0) = 1.0
                DO L = MAX(1,LNUMSG-1-INT((LLIFG/PHINTS))),LNUMSG+1 
                    IF (LNUMSG.LT.LNUMX) THEN
                        LATL4(L)= LATL3(L) * NFLF2(0)            
                        NFLF2(L) = AMIN1(1.0,NFLF2(L) + AMAX1(0.0,NFLF2(0)) * (LATLPOT(L)-LATLPREV(L))/LAPOTX(L))
                    ENDIF  
                ENDDO
                
            ENDIF
    
        ELSE     ! ISWNIT = N   
    
            LATL4 = LATL3
            NFLF2 = 1.0            
    
        ENDIF    ! End of N uptake and growth adjustmenets
    END SUBROUTINE CS_Growth_NUptake