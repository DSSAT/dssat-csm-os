!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5875 - 5924 of the original CSCAS code. The names of the 
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
        
        IMPLICIT NONE
        
        INTEGER NLAYR 
        REAL BRSTAGE
        
        !-----------------------------------------------------------------------
        !         Update nitrogen amounts
        !-----------------------------------------------------------------------
        NUPC = NUPC + NUPD                                                                                          !EQN 202
        LEAFNEXCESS = 0.0
        IF (LANC.GT.LNCX) LEAFNEXCESS = (LFWT-SENLFG-SENLFGRS)*(LANC-LNCX)                                          !EQN 245 !LPM 25OCT2015 to consider N by cohort LANC has to be by cohort
        LEAFN = LEAFN + GROLSRTN + LNUSE(0) - SENNLFG - SENNLFGRS - lnph - LEAFNEXCESS                              !EQN 242
        LNPHC = LNPHC +  LNPH                                                                                       !EQN 423
        IF (LEAFN.LT.1.0E-10) LEAFN = 0.0
        STEMNEXCESS = 0.0
        plant%STEMNEXCESSN = 0.0
        !IF (SANC.GT.SNCX) STEMNEXCESS = (STWT+CRWT)*(SANC-SNCX)                                                     !EQN 246
        !STEMN = STEMN + SNUSE(0) - SNPH - STEMNEXCESS                                                               !EQN 247
        
        DO BR = 0, BRSTAGE                                                                                                                                                          
            DO LF = 1, LNUMSIMSTG(BR)                                                                            !LPM23MAY2015 To consider different N demand by node according with its age   
                IF (plant(BR,LF)%SANC < plant(BR,LF)%SNCX) THEN 
                    plant(BR,LF)%STEMNEXCESSN = (plant(BR,LF)%NODEWT*(STWT+CRWT)/(STWTP+CRWTP))*(plant(BR,LF)%SANC-plant(BR,LF)%SNCX)
                    STEMNEXCESS = STEMNEXCESS + plant(BR,LF)%STEMNEXCESSN
                    plant(BR,LF)%STEMNN = plant(BR,LF)%STEMNN + SNUSEN(0,BR,LF)-plant(BR,LF)%SNPHN- plant(BR,LF)%STEMNEXCESSN  
                    STEMN = STEMN + plant(BR,LF)%STEMNN
                ENDIF 
            ENDDO
        ENDDO
           
        
        SNPHC = SNPHC +  SNPH                                                                                       !EQN 248
        IF (STEMN < 1.0E-10) STEMN = 0.0
        ROOTNS = 0.0
        SENNGS = 0.0
        DO L = 1, NLAYR
            SENNL(L) = SENNL(L) + RTNSL(L)                                                                             !EQN 249
            ROOTNS = ROOTNS + RTNSL(L)                                                                                 !EQN 250
            SENNS = SENNS + RTNSL(L)                                                                                   !EQN 251
            SENNGS = SENNGS + RTNSL(L)                                                                                 !EQN 252
        END DO
        ! LAH Maybe also need LEAFNEXESS if LANC > LNCX
        ROOTNEXCESS = 0.0
        IF (RANC.GT.RNCX) ROOTNEXCESS = (RTWT-(SENWALG(L)/(PLTPOP*10.0)))*(RANC-RNCX)                                  !EQN 253
        ROOTN = ROOTN + (RNUSE(0)-ROOTNS-GROLSRTN) - ROOTNEXCESS                                                       !EQN 254
        SROOTN = SROOTN + SRNUSE(0)                                                                                    !EQN 255
        SEEDN = SEEDN - SEEDNUSE - SEEDNUSE2                                                                           !EQN 204
        IF (SEEDN.LT.1.0E-6) SEEDN = 0.0
        RSN = RSN - RSNUSED + SENNLFGRS - RSNPH + LEAFNEXCESS + STEMNEXCESS + ROOTNEXCESS                              !EQN 157
        RSNPHC = RSNPHC +  RSNPH                                                                                       !EQN 256
        SENNL(0) = SENNL(0) + SENNLFG                                                                                  !EQN 257
        
        HPRODN = SROOTN
        
        ! Harvest index for N
        HIND = 0.0
        IF ((LEAFN+STEMN+RSN) > 0.0) THEN 
            HIND = HPRODN/(LEAFN+STEMN+HPRODN+RSN)                                           !EQN 258
        ENDIF
    
    END SUBROUTINE YCA_Integ_N
        
