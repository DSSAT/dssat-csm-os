!***************************************************************************************************************************
! This is the code from the section (DYNAMIC == INTEGR) lines 9574 - 9620 of the original CSCAS code. The names of the 
! dummy arguments are the same as in the original CSCAS code and the call statement and are declared here. The variables 
! that are not arguments are declared in module YCA_First_Trans_m. Unless identified as by MF, all comments are those of 
! the original CSCAS.FOR code.
!
! SUBROUTINE YCA_Out_ReInit reinitializes some variables and stores others for possible use next day/step. 
! Note that the reinitialization is conditional, but storage always takes place.
!***************************************************************************************************************************
    
    SUBROUTINE YCA_Out_ReInit ( & 
        BRSTAGE     , CN          , DYNAMIC     , NFP         , RNMODE      , SRAD        , STGYEARDOY  , &
        UNH4        , UNO3        , LAI         &
        )

! 2023-01-25 chp removed unused variables
!       IDETL       , 

        USE ModuleDefs
        USE YCA_First_Trans_m
        USE YCA_Formats_m
     
        IMPLICIT NONE 
     
        INTEGER :: CN          , DYNAMIC     , STGYEARDOY(0:19)
        
        REAL    :: BRSTAGE     , NFP         , SRAD        , UNH4(NL)     , UNO3(NL)    , LAI
        
        CHARACTER(LEN=1)  :: RNMODE      ! IDETL       , 

        !INTEGER,PARAMETER::SEASEND= 6 ! Program ending indicator                             ! MF In ModuleDefs 
        
        !***************************************************************************************************************
        IF (STGYEARDOY(PSX+1) == YEARDOY .OR.DYNAMIC == SEASEND) THEN         ! If harvest/failure day
        !***************************************************************************************************************
            !-----------------------------------------------------------------------------------------------------------
            !            Re-initialize
            !-----------------------------------------------------------------------------------------------------------
            !Need to re-initialize following because of automatic
            !fertilization routines in DSSAT
            NFG = 1.0
            NFP = 1.0
            WFG = 1.0
            WFP = 1.0
                
            UNO3 = 0.0
            UNH4 = 0.0
            
            SEASENDOUT = 'Y'
                
        ENDIF ! End STGYEARDOY(PSX) == YEARDOY.OR.DYNAMIC == SEASEND
        
        !---------------------------------------------------------------------------------------------------------------
        !       Store variables for possible use next day/step
        !---------------------------------------------------------------------------------------------------------------
        
        CNADPREV = CNAD
        IF (CN <= 1) CROPPREV = CROP
        IF (RNMODE /= 'G') CUDIRFLPREV = CUDIRFLE
        CWADPREV = CWAD
        ECONOPREV = ECONO
        EMRGFRPREV = EMRGFR
        GESTAGEPREV = GESTAGE
        BRSTAGEPREV = BRSTAGE
        LAIPREV = LAI
        LNUMPREV = LNUM
        PARIPREV = PARI
        PLYEARDOYPREV = PLYEARDOY
        BRNUMSTPREV = BRNUMST
        SPDIRFLPREV = SPDIRFLE
        SRADPREV = SRAD
        SRWTFPREV = SRWTF
        SRWTPREV = SRWT
        !SWFRPREV = SWFR !LPM 06JUN2015 SWFR is not used 
        VARNOPREV = VARNO
        YEARDOYPREV = YEARDOY
    
    END SUBROUTINE YCA_Out_ReInit 
            

