!----------------------------------------------------------------------!
!  SUBROUTINE WNOSTRESS                                                !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date  : December 2001                                               !
!  Author: B.A.M. Bouman                                               !
!                                                                      !
!  Purpose: Invoked when production environment is POTENTIAL.          !
!           Sets actual transpiration of a crop at zero, and sets      !
!           effects of water stress on growth and development of rice  !
!           at unity.                                                  !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! NL      I4  Number of soil layers (-)                             I  !
! TRW     R4  Actual transpiration rate (mm d-1)                    O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for accelerating leaf death (-)         O  !
! LESTRS  R4  Stress factor for reducing expansion of leaves (-)    O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
! ZRT     R4  Root depth (m)                                           !
!----------------------------------------------------------------------!
      SUBROUTINE WNOSTRESS (NL, TRW, TRWL, ZRT, TKL, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)

      USE public_module
      IMPLICIT NONE
!-----Formal parameters
      INTEGER NL
      REAL    TRW, LRSTRS, LDSTRS, LESTRS, PCEW, CPEW
      REAL    TRWL(NL),theTRW, ZRLT, ZZL,TKL(NL),ZRT
!-----Local variables
      INTEGER I
      SAVE         !&#@TAOLI

        theTRW = TRW   !TAOLI 17 AUG, 2010
      TRW    = 0.
      LRSTRS = 1.
      LDSTRS = 1.
      LESTRS = 1.
      CPEW   = 1.
      PCEW   = 1.
        ZZL=0.
        IF(ZRT.EQ.0.0) THEN
            TRWL=0.0;TRWL(1)=THETRW
        ELSE
            DO I=1,NL                  
!        TRWL(I) = 0.
            !EQUAVLENT SPLIT TRC INTO ROOT ZONE, TAOLI, 17 AUG 2010
                  ZRLT=MAX(0.0,MIN(ZRT-ZZL,TKL(I)))
                  TRWL(I)=THETRW/ZRT*ZRLT
                  PV%PTRWL(I)=TRWL(I)
                  ZZL=ZZL+ZRLT  
            END DO
        ENDIF
      RETURN
      END

!------------------------------------------------------------------------------------------
!This routine is used to get necessary soil information when the potential water is applied
!Developed: Dr. Tao li, 7 April 2011
!------------------------------------------------------------------------------------------      
      SUBROUTINE POTENTIAL_SOIL(ITASK, IUNITD, IUNITL, FILEI2, OUTPUT, NITROENV, &
                      NL, TKL, TKLT, WCAD, WCWP, WCFC, WCST, WCLQT, WL0)
      use public_module
        IMPLICIT NONE
!---- Formal parameters
      INTEGER       ITASK,IUNITD,IUNITL,NL
      LOGICAL       OUTPUT
      CHARACTER (*) FILEI2, NITROENV
      REAL          TKLT,WL0
      
      INTEGER      MNL
      PARAMETER   (MNL=10)
      REAL         TKL(MNL),WCAD(MNL),WCWP(MNL),WCFC(MNL),WCST(MNL),WCLQT(MNL)
      LOGICAL RDINQR
      INTEGER I
      
      CALL UPPERC(NITROENV)               
      IF (ITASK.EQ.1) THEN
      !When water and nitrogen both are in potential, may not have soil file        
        IF(INDEX(NITROENV, "POTENTIAL")) THEN   !MAKE VIRTUAL SOIL
            TKL=1.0; WCST=0.0;WCLQT = 0.0
            WCFC=0.0;WCWP=0.0;WCAD = 0.0  
            NL=1; TKL(1)=1.0; TKLT=1.0; WL0 = 0.0
            WCST(1)=0.50;WCLQT(1) = WCST(1)
            WCFC(1)=0.35;WCWP(1)=0.18;WCAD(1)=0.01        
        ELSE                                        !USE EXISTED SOIL FILE            
             TKLT = SUM(TKL(1:NL)); WL0 =0.0            
        END IF
    Else     !modified to force potential condition, TAOLI, 7 Sept 2012
        !SAVE THE VALUES INTO PUBLIC VARIABLES
        PV%PNL = NL
        DO I=1, NL
            PV%PDLAYER(I) = TKL(I)*1000.0    !CONVERT INTO mm
            pv%pwcst(i) = wcst(i);                        pv%pwcfc(i) = wcfc(i)
                pv%pwcwp(i) = wcwp(i);                        pv%pwcad(i) = wcad(i);
                IF(INDEX(NITROENV,'POTENTIAL').GT.0) THEN  
                    WCLQT(I) =0.9*WCST(I)+0.1*WCFC(I)
                ELSE
                    WCLQT(I) =WCST(I)
                END IF                
                IF(PV%PBD(I).LE.0.0) THEN
                   pv%pbd(i)= 2.65*(1-PV%PWCST(I))
                ENDIF 
        END DO
      END IF
      RETURN               
      END SUBROUTINE POTENTIAL_SOIL

