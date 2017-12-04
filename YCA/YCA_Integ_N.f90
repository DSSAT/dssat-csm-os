!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 5875 - 5924 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine YCA_Integ_N updates nitrogen amounts. 
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Integ_N ( &
        NLAYR    , BRSTAGE   & 
        )
        
        USE YCA_First_Trans_m
        USE YCA_Control_Plant
        USE YCA_Control_Leaf
        
        IMPLICIT NONE
        
        INTEGER NLAYR 
        INTEGER :: BR                      ! Index for branch number/cohorts#          ! (From SeasInit)  
        INTEGER :: LF                      ! Loop counter leaves            #          !LPM 21MAR15 to add a leaf counter
        REAL BRSTAGE
        
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        NUPC = NUPC + NUPD                                                                                          !EQN 202
        LeafNEXCESS = 0.0
        IF (LANC > LNCX) THEN
            LeafNEXCESS = (LFWT-leafTotalSenescedWeight())*(LANC-LNCX)                                          !EQN 245 !LPM 25OCT2015 to consider N by cohort LANC has to be by cohort
        ENDIF
        LeafN = LeafN + StemLeafGrowthRTN + LNUSE(0) - SENNLFG - SENNLFGRS - lnph - LeafNEXCESS                              !EQN 242
        LNPHC = LNPHC +  LNPH                                                                                       !EQN 423
        IF (LeafN < 1.0E-10) THEN 
            LeafN = 0.0
        ENDIF
            
        StemNExcess = 0.0
        node%StemNExcessByNode = 0.0
        !IF (SANC > SNCX) StemNExcess = (woodyWeight())*(SANC-SNCX)                                                     !EQN 246
        !StemN = STEMN + SNUSE(0) - SNPH - StemNExcess                                                               !EQN 247
        
        DO BR = 0, BRSTAGE                                                                                                                                                          
            DO LF = 1, LNUMSIMSTG(BR)                                                                            !LPM23MAY2015 To consider different N demand by node according with its age   
                IF (node(BR,LF)%SANC < node(BR,LF)%SNCX) THEN 
                    node(BR,LF)%StemNExcessByNode = (node(BR,LF)%CohortWeight*(woodyWeight())/(StemWeightP+CRWTP))*(node(BR,LF)%SANC-node(BR,LF)%SNCX)
                    StemNExcess = StemNExcess + node(BR,LF)%StemNExcessByNode
                    node(BR,LF)%StemNByNode = node(BR,LF)%StemNByNode + ShootNUseByNode(0,BR,LF) - node(BR,LF)%StemNHarvstByNode - node(BR,LF)%StemNExcessByNode  
                    
                    StemN = StemN + node(BR,LF)%StemNByNode
                     
                ENDIF 
            ENDDO
        ENDDO
           
        
        SNPHC = SNPHC +  SNPH                                                                                       !EQN 248
        IF (StemN < 1.0E-10) StemN = 0.0
        RootNS = 0.0
        SENNGS = 0.0
        DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)                                                                             !EQN 249
            RootNS = RootNS + RTNSL(L)                                                                                 !EQN 250
            SENNS = SENNS + RTNSL(L)                                                                                   !EQN 251
            SENNGS = SENNGS + RTNSL(L)                                                                                 !EQN 252
        END DO
        ! LAH Maybe also need LeafNEXESS if LANC > LNCX
        RootNEXCESS = 0.0
        IF (RANC > RNCX) RootNEXCESS = (RTWT-(SENWALG(L)/(plantPopulation())))*(RANC-RNCX)                                  !EQN 253
        RootN = RootN + (RNUSE(0)-RootNS-StemLeafGrowthRTN) - RootNEXCESS                                                       !EQN 254
        SRootN = SRootN + SRNUSE(0)                                                                                    !EQN 255
        SEEDN = SEEDN - SEEDNUSE - SEEDNUSE2                                                                           !EQN 204
        IF (SEEDN < 1.0E-6) SEEDN = 0.0
        RSN = RSN - RSNUSED + SENNLFGRS - RSNPH + LeafNEXCESS + StemNExcess + RootNEXCESS                              !EQN 157
        RSNPHC = RSNPHC +  RSNPH                                                                                       !EQN 256
        SENNL(0) = SENNL(0) + SENNLFG                                                                                  !EQN 257
        
        HPRODN = SRootN
        
        ! Harvest index for N
        HIND = 0.0
        IF ((LeafN+StemN+RSN) > 0.0) THEN 
            HIND = HPRODN/(LeafN+StemN+HPRODN+RSN)                                           !EQN 258
        ENDIF
    
    END SUBROUTINE YCA_Integ_N
        
