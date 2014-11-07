!***************************************************************************************************************************
! This is the code from the section (DYNAMIC.EQ.INTEGR) lines 5875 - 5924 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in Module_CSCAS_Vars_List. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! Subroutine CS_Integ_N updates nitrogen amounts. 
!***************************************************************************************************************************
    
    SUBROUTINE CS_Integ_N ( &
        NLAYR       & 
        )
        
        USE ModuleDefs
        USE Module_CSCAS_Vars_List
        
        IMPLICIT NONE
        
        INTEGER NLAYR           
        
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
    
    END SUBROUTINE CS_Integ_N
        
