!----------------------------------------------------------------------!
!  SUBROUTINE WSTRESSs                                                  !
!  Used in ORYZA2000 model version 1.0                                 !
!  Date   : Feburary 2010                                              !
!  Author : Modified from B.A.M. Bouman by Tao Li                                              !
!  Version: 1.0 (Based on earlier versions of DSTRES)                  !
!  Version: Redistribution of transpiration uptake per soil layer.     !
!           Avarage stress factors over all soil layers                !
!           Calculations based on Wopereis et al (1996a)               !
!       Modifications:                                                                                                                                                              !
!            1. The water uptake choices: 
!      1) Use old one when root growth do not be calulated                                     !
!      2) Use new one developed from MB Coelho, FJ Villalobos, L Mateos! 
!         Agricultural Water Management, 60, 2003, 99-118. Modeling    ! 
!                              root growth and the soil-planrt-atmosphere continuum of              !
!         cotton crops                                                                                     !
!         Then quantifying drought stress by                           !
!        (1) Actual/demand transpiration                               !
!        (2) Fraction of transpirable soil water content               !
!                                                                      !
!  Purpose: Calculate actual transpiration of a crop, and the  effects !
!          of water stress on growth and development of rice.          !
!                                                                      !
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      !
! name   type meaning (unit)                                     class !
! ----   ---- ---------------                                    ----- !
! ITASK   I4  Task that subroutine should perform (-)               I  !
! DELT    R4  Time step of integration (d)                          T  !
! OUTPUT  R4  Flag to indicate if output should be done (-)         I  !
! IUNITD  I4  Unit that can be used for input files (-)             I  !
! IUNITL  I4  Unit number for log file messages (-)                 I  !
! FILEI1  C*  Name of file with input model data (-)                I  !
! TRC     R4  Potential transpiration rate (mm d-1)                 I  !
! ZRT     R4  Rooting depth (m)                                     I  !
! TKL     R4  Array of thicknesses soil layers (m)                  I  !
! NL      I4  Number of soil layers (-)                             I  !
! CROPSTA I4  Crop stage (-)                                        I  !
! WCLQT   R4  Array of actual soil water contents/layer (m3 m-3)    I  !
! WCWP    R4  Array of water content at wilting point/layer (m3 m-3)I  !
! WCAD    R4  Array of water content air dry/ layer (m3 m-3)        I  !
! MSKPA   R4  Array with soil water potential/layer (KPa)           I  !
! TRW     R4  Actual transpiration rate (mm)                        O  !
! TRWL    R4  Array of actual transpiration rate/layer (mm d-1)     O  !
! LRSTRS  R4  Stress factor for rolling of leaves (-)               O  !
! LDSTRS  R4  Stress factor for dead leaves (-)                     O  !
! LESTRS  R4  Stress factor for expansion of leaves (-)             O  !
! PCEW    R4  Stress factor for CO2 assimilation (-)                O  !
!                                                                      !
! Files included: -                                                    !
!                                                                      !
!----------------------------------------------------------------------!
MODULE WSTRESS_MODULE            !SPECIAL FOR INTEGRATE THE AVALIABLE WATER FOR LOWER THAN FIELD CAPACITY
       REAL FOSMATIC1, FWCWP1, FWCFC1 
END MODULE WSTRESS_MODULE
      SUBROUTINE WSTRESS2 (ITASK,  DELT,   OUTPUT, IUNITD, IUNITL, FILEI1, FILEIT, &
                          TRC,    ZRT,    TKL,    NL,    CROPSTA, &
                          WCLQT,  WCWP,   MSKPA, &
                          TRW,    TRWL,   LRSTRS, LDSTRS, LESTRS, PCEW, CPEW)


      !USE CHART
      USE ROOTGROWTH
      USE public_module
      use Module_OutDat
      IMPLICIT NONE

!-----Formal parameters
      INTEGER ITASK, IUNITD, IUNITL, NL, CROPSTA
      REAL    DELT, TRC, ZRT 
      REAL    TRW, LRSTRS, LDSTRS,PCEW, CPEW, LESTRS
      REAL    TKL(NL),WCLQT(NL),WCWP(NL)
      REAL    MSKPA(NL)
      LOGICAL OUTPUT, RDINQR
      CHARACTER (*) FILEI1, FILEIT 

!-----Local variables
!     The following parameters must be declared with INL=10 because
!     they are used/defined in COMMON blocks! (see PADDY subroutines)
      INTEGER    I,INL, J, I1
      PARAMETER (INL=10)
      REAL       TRWL(INL), LR(INL), LE(INL), LD(INL), TRR(INL),WLA(INL)
      REAL       TRRM, ZRTL, ZLL , LRAV, LEAV, LDAV
      REAL       ULLS, LLLS, ULDL, LLDL, LLLE, ULLE, LLRT, ULRT
              REAL TRUPAC, TROOTF, TWATERF  !TAOLI, 26 FEB 2010
              REAL FSWTD, OSMATA1    !TAOLI, 26 FEB 2010
              REAL, ALLOCATABLE::RUPAC1(:), RUPAC2(:), RUPAC3(:), RUPAC4(:) !TAOLI, 26 FEB 2010
              REAL Fintegration
      REAL LIMIT 
      REAL TINY
      PARAMETER (TINY=0.0000000001)

!      REAL NOTNUL
!      CHARACTER (10) SWIRTR
      INTEGER SWIRTR        !MODIFIED BY TAOLI, 28MAY 2011
        CHARACTER (32) ESTABL            !The establishment methods of rice in the field
        REAL SWIRTRF,XXXX

      SAVE         !&#@TAOLI
 
      IF (NL.GT.INL) CALL FATALERR ('STRESS','too many soil layers')

!====================================================================!
! INITIALIZATION (ITASK=1)                                           !
!====================================================================!

      IF (ITASK.EQ.1) THEN
!        Open crop input file
         CALL RDINIT(IUNITD,IUNITL,FILEI1)
!         CALL RDSCHA ('SWIRTR', SWIRTR)
         CALL RDSINT ('SWIRTR', SWIRTR)
!             CALL UPPERC (SWIRTR)
!         IF (SWIRTR .NE. 'DATA' .AND. SWIRTR .NE. 'FUNCTION') THEN
         IF (SWIRTR .NE. 1 .AND. SWIRTR .NE. 2 .AND. SWIRTR.NE.3) THEN
            CALL FATALERR ('Crop data file','Unknown name for SWIRTR')
         END IF 
!        Read initial states (Note: values in kPa)
         CALL RDSREA ('ULLS', ULLS)
         CALL RDSREA ('LLLS', LLLS)
         CALL RDSREA ('ULDL', ULDL)
         CALL RDSREA ('LLDL', LLDL)
         CALL RDSREA ('LLLE', LLLE)
         CALL RDSREA ('ULLE', ULLE)
         IF(SWIRTR.EQ.1) THEN
             CALL RDSREA ('LLRT', LLRT)
             CALL RDSREA ('ULRT', ULRT)
             ELSEIF(SWIRTR.EQ.2) THEN            !ADDED BY TAOLI, 31 MAY, 2010
                  IF(RDINQR('SWIRTRF')) THEN
                        CALL RDSREA('SWIRTRF',SWIRTRF) !IF SETTING IN FUNCTION, 
                  ELSE
                        SWIRTRF=0.003297                  !IT MEANS THERE IS 5% REDUCTION AT 10 kPa.
                  ENDIF
             ELSEIF(SWIRTR.EQ.3) THEN
                 IF(RDINQR('FSWTD')) THEN
                      CALL RDSREA('FSWTD', FSWTD)
                      IF(FSWTD.GT.0.0) THEN
                          FSWTD = 1.0/FSWTD
                      ELSE
                          FSWTD = 2.42
                      END IF
                 ELSE
                      FSWTD = 2.42
                 ENDIF
         ENDIF
         CLOSE (IUNITD)
             CALL RDINIT(IUNITD, IUNITL, FILEIT)
                  CALL RDSCHA('ESTAB',ESTABL)
                  CALL UPPERC(ESTABL)      
             CLOSE(IUNITD)

         LRAV   = 1.
         LDAV   = 1.
         LEAV   = 1.
         LESTRS = 1.
         PCEW   = 1.
         CPEW   = 1.
         LRSTRS = 1.
         LDSTRS = 1.
         DO I=1,NL
           TRWL(I) = 0.
         END DO
         TRW = 0.
!====================================================================!
! RATE CALCULATION SECTION  (ITASK=2)                                !
!====================================================================!

    ELSE IF (ITASK.EQ.2) THEN

!--- for transplanting, there is no water stress in the seed bed, but direct seeding has
!----CHANGED BY TAOLI, 4 JUNE, 2010
        IF(ESTABL.EQ.'TRANSPLANT') THEN
            I1 = 2
        ELSEIF(ESTABL.EQ.'DIRECT-SEED') THEN
            I1 = 1
        ENDIF
      IF (CROPSTA .GT. I1) THEN
           ZLL = 0.0
           LRAV = 0.
           LEAV = 0.
           LDAV = 0.
              !We use the same function for leave rolling, death and expansion under drought 
              !but we use different function to quantify the effects on transpiration as 
              !well the photosynthesis. TAOLI 26 FEB 2010
            
           DO I = 1,NL
!-------------Root length in each soil layer
              ZRTL  = MIN(TKL(I),MAX((ZRT-ZLL),0.0))

!-------------Leaf-rolling factor
              LR(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLLS)) &
                            /(LOG10(ULLS)-LOG10(LLLS))
              LR(I) = LIMIT(0.,1.,LR(I))
              LRAV  = LRAV+(ZRTL/(ZRT+TINY))*LR(I)

!-------------Relative leaf expansion rate factor
              LE(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLLE)) &
                            /(LOG10(ULLE)-LOG10(LLLE))
              LE(I) = LIMIT(0.,1.,LE(I))
              LEAV  = LEAV+(ZRTL/(ZRT+TINY))*LE(I)

!-------------Relative death rate factor
              LD(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLDL)) &
                            /(LOG10(ULDL)-LOG10(LLDL))
              LD(I) = LIMIT(0.,1.,LD(I))
              LDAV  = LDAV+(ZRTL/(ZRT+TINY))*LD(I)
                    ZLL     = ZLL+TKL(I)
          ENDDO
          !-----Set stress factors as average over all layers: DEFAULT
           LRSTRS = LRAV  !TAOLI 22Feb 2012
           LDSTRS = LDAV  !TAOLI 22Feb 2012
           LESTRS = LEAV  ! TAOLI 22Feb 2012
        !TAOLI 26 FEB 2010   
       IF(SWIRTR.EQ.1 .OR. SWIRTR.EQ.2) THEN
                TRRM = TRC/(ZRT+1.0E-10)
                    TRW  = 0.
                    ZLL  = 0.
          DO I = 1, NL
              ZRTL  = MIN(TKL(I),MAX((ZRT-ZLL),0.0))
!-------------Relative transpiration ratio (actual/potential)
              IF (MSKPA(I) .GE. 10000.) THEN
                 TRR(I) = 0.
              ELSE
                 IF (SWIRTR .EQ. 1) THEN
                    TRR(I) = (LOG10(MSKPA(I)+TINY)-LOG10(LLRT)) &
                                  /(LOG10(ULRT)-LOG10(LLRT))
                    TRR(I) = LIMIT(0.,1.,TRR(I))
                 ELSE
                    !TRR(I)  = 2./(1.+EXP(0.003297*MSKPA(I)))
                              TRR(I)  = 2./(1.+EXP(SWIRTRF*MSKPA(I))) !changed by TAOLI, 2010
                 END IF
              END IF
              TRR(I)  = LIMIT(0.,1.,TRR(I))
              WLA(I)  = MAX(0.0,(WCLQT(I)-WCWP(I))*ZRTL*1000.)
              TRWL(I) = MIN(TRR(I)*ZRTL*TRRM,WLA(I)/DELT)
              TRW     = TRW + TRWL(I)
              ZLL     = ZLL+TKL(I)
           END DO

!-----Compensation of water extraction from soil layers if drought stress occurs
!     Take water from soil layer that has a surplus, starting from top.
           DO I = 1,NL
              IF (TRW .LT. TRC) THEN
                 IF ((TRR(I) .GE. 1) .AND. (TRWL(I) .LT. WLA(I)/DELT)) THEN
                    TRWL(I) = MIN(WLA(I)/DELT,(TRWL(I)+(TRC-TRW)/DELT))
                 END IF
                 TRW = 0.
                 DO J = 1,NL
                   TRW = TRW + TRWL(J)
                 END DO
              END IF
           END DO
!----------update public variables for soil water uptake
                  DO J = 1,NL
                pv%PTrwl(j)= TRWL(J)
            END DO
           PCEW   = (TRW/TRC)
       ELSEIF(SWIRTR.EQ.3) THEN   !TAOLI 26 FEB 2010
        ALLOCATE(RUPAC1(NL), RUPAC2(NL), RUPAC3(NL), RUPAC4(NL))
        
        OSMATA1 = FSWTD/1.5; RUPAC1(:)=0.0; RUPAC2(:)=0.0; RUPAC3(:)=0.0; RUPAC4(:)=0.0
       
        TROOTF =0.0; TWATERF = 0.0; ZLL = 0.0
        DO I=1, NL                         
                   ZRTL  = MIN(TKL(I), MAX((ZRT-ZLL),0.0))!MAX((pv%prootad-ZLL),0.0)) 
               TROOTF =TROOTF + ROOTC(I)
                   IF(ZRTL.GT.0.0) THEN            !!
               IF(WCLQT(I) .GT. PV%PWCFC(I)) THEN
                    RUPAC3(I) = ((WCLQT(I) - PV%PWCFC(I)) + Fintegration(PV%PWCWP(I), PV%PWCFC(I), OSMATA1, PV%PWCFC(I))) 
                    RUPAC3(I) =RUPAC3(I)*1000.0*ZRTL !TAOLI 27 Aug 2010
                    TWATERF = TWATERF + RUPAC3(I)
               ELSEif((WCLQT(I) .LE. PV%PWCFC(I)) .and. (WCLQT(I) .GT. PV%PWCWP(I))) then
                    RUPAC3(I) = (Fintegration(PV%PWCWP(I), PV%PWCFC(I), OSMATA1,WCLQT(I)))  !TAOLI 27 Aug 2010
                    RUPAC3(I) =RUPAC3(I) *1000.0*ZRTL
                    TWATERF = TWATERF + RUPAC3(I)
                ELSE
                     RUPAC3(I) = 0.0
               ENDIF
                   ELSE            !!
                        RUPAC3(I) = 0.0            !!
                   ENDIF            !!
                   ZLL = ZLL+TKL(I)              
         ENDDO
         TRUPAC = 0.0;XXXX=TWATERF
         IF((TROOTF.GT.0.0).AND.(TWATERF.GT.0.0)) THEN    
             DO I = 1, NL            
                   RUPAC2(I) = ROOTC(I)/TROOTF      
                   RUPAC1(I) = RUPAC3(I)/ TWATERF
                      TRUPAC = TRUPAC + RUPAC1(I)*RUPAC2(I)  
             ENDDO
             TWATERF = TRUPAC; TRUPAC = 0.0
         ELSE
            TWATERF = 0.0; TRUPAC = 0.0
         END IF
             !CALCULATE TOTAL AVALIABLE WATER CAN BE UPTAKEN
         IF(TWATERF.GT.0.0) THEN
             DO I = 1, NL
                      RUPAC4(I)=RUPAC3(I) * RUPAC2(I)*RUPAC1(I)/TWATERF
                      TRUPAC = TRUPAC + RUPAC4(I)             
             ENDDO
                 TRUPAC = TRUPAC !*TRC      !AFTER DECEMBER 2011
              ELSE
                 TRUPAC = 0.0; RUPAC4(:)= 0.0
              END IF
             !---------------------------------------------------------------------------------------------------
             !This algorithm is modified from Coelho M.B. et al., 2003. Modeling root growth and the soil-plant-atmosphere continuum 
             !of cotton crops, Agricultural Water Management 60, 99-118.
         !DETERMINE THE FINAL TOTAL UPTAKE, SOIL WATER LOSE IN EACH LAYER
         IF(TRC.GT.0.0) THEN
               IF(TRC.GE.TRUPAC) THEN
                     TRW = TRUPAC
                     DO I = 1, NL
                           TRWL(I) = RUPAC4(I)/DELT
                     ENDDO
            ELSE
                     TRW =TRC; TROOTF = TRC/TRUPAC
                     DO I = 1, NL
                           TRWL(I) = RUPAC4(I) * TROOTF/DELT
                     ENDDO
            ENDIF
         ELSE
            TRW = 0.0; TRWL(:) =0.0
         ENDIF
!--------UPDATE the soil water uptake
             DO I = 1, NL
                  pv%PTRWL(i) = TRWL(i)
         ENDDO
      
         !Calculating the drought stress factors on transpiration, temporal function, will use GECROS function
             PCEW   =min(1.0, max(0.0,1.0-max(0.0,1.0 - MAX(0.0001,TRW)/MAX(0.0001,TRC))**(FSWTD/1.5)))                              ! After 27 Aug, 2010
             DEALLOCATE(RUPAC1, RUPAC2, RUPAC3, RUPAC4)   
       endif
!-------If crop is not in the main field, set all stres factors at 1.
     ELSE
          PCEW   = 1.
          LRSTRS = 1.
          LDSTRS = 1.
          LESTRS = 1.
              TRW = 0.
              !---calculate the TRWL under TRC, weighted only by root mass
              TROOTF =0.0
              DO I=1, NL
                  TROOTF = TROOTF + ROOTC(I)
              END DO
              
              DO I = 1, NL
                  IF(TROOTF.GT.0.0) THEN
                        TRWL(I) = TRC*ROOTC(I)/TROOTF
                  ELSE
                        TRWL(I) = 0.0
                  ENDIF
                  TRW =TRW +TRWL(I)
              ENDDO
         END IF

        CPEW = LESTRS
!------Output writing 
      IF (OUTPUT) THEN
         IF (CROPSTA .GE.1) THEN
            CALL OUTDAT (2, 0, 'TRW' , TRW)
         END IF
      END IF
  elseif(ITASK.eq.4) then

  END IF

  RETURN

END

real function FIntegration(theWCWP, theWCFC, theOSMATA, theUP)
use wstress_module
implicit none

!formal parameters
real, intent(in) :: theWCWP, theWCFC, theOSMATA, theUP

real     :: ErrRest
integer  :: NrEvaluations, EvalStatus
integer, parameter :: Key = 2 ! 10 Gauss points, 21 Gauss-Kronrod points
real     :: F
external :: F

! Set error tolerances
real, parameter :: ErrAbs = 0.0
real, parameter :: ErrRel = 0.001

FOSMATIC1 = theOSMATA; FWCWP1 = theWCWP; FWCFC1 = theWCFC

call qag(F, theWCWP, theUP, ErrAbs, ErrRel, Key, &
         FIntegration, ErrRest, NrEvaluations, EvalStatus)

if (EvalStatus /= 0) then
  !call fatalerr('FIntegration', 'qag routine could not achieve required accuracy')
  FIntegration = max(0.0,FIntegration)
end if

end function
!
REAL FUNCTION F(X)
use WSTRESS_MODULE
REAL X
F = (1.0-((FWCFC1-x)/(FWCFC1-FWCWP1))**FOSMATIC1)
RETURN
END

subroutine EDROGHT(RPAR1,EP,LAI,WND,VP,TA,PCEW,KDF1,RBH,RT,RBW,RSWP,RSWA)
!*--------------------------------------------------------------------------------*
!* This routine is used to calculate the potential and actual stomatal resistance *
!* Developed by Dr. TaoLi, 29 March, 2010                                                              *
!* Variables                                                                                                        *
!* Name            Type      Meaning            Unit          Class *
!* RPAR            R4            The absorbed radiation                                    J m-2 s-1      I        *
!* LAI      R4            The leaf area index                         --          I     *
!* EP            R4            Potential transpiration                                    mm s-1      I     *
!* WND      R4            Wind speed                                                      m s-1            I        *
!* VP       R4      Vapor pressure                                                KPa         I     *
!* TA       R4      Daily average temperature                   oC          I     *
!* PCEW            R4      The scaling factor for Transp. and Photos.  --          I/O   *
!* VPS            R4            The saturated vapor pressure                        KPa                  -     *
!* VPSL     R4      The slope of the curve of saturate VP change (KPa/oC)   --    *
!* VHCA            R4            Volumetric heap capacity of air             j m-3 oC-1  --    *
!* RBH            R4            Leaf boundary layer resistance to heat            s m-1            I        *
!* RT            R4            Turbulence resistance                                    s m-1            I        *
!* RBW            R4            Leaf boundary layer to water                s m-1       I     *
!* VPD      R4            Vapour pressure deficit                     KPa         --    *
!* RSWP            R4      Potential stomatal resistance                        s m-1            O     *
!* RSWA            R4            Actual stomatal resistance                  s m-1       O     *
!*--------------------------------------------------------------------------------*
    IMPLICIT NONE
      REAL RPAR1,EP,LAI,WND,VP,TA,PCEW,KDF1
      REAL VPS, VPSL, LHVAP,PSYC, EA,RSWP1
      REAL RPAR, vhca, rswp, rswa, vpd, rbh, RT, RBW

      VHCA   = 1240.            !volumetric heat capacity (J/m3/oC)
      LHVAP  = 2.4E6            !latent heat of water vaporization(J/kg)
      PSYC = 0.067
      RSWP=0.0;RSWP1=0.0;RSWA=0.0
      RPAR = RPAR1*(1.0-exp(-KDF1*LAI))
      RPAR = RPAR/LAI
      !calculate the saturate vapor pressure and the slope
      CALL SVPS1 (TA, VPS, VPSL)
      VPD = VPS - VP
      !calculate the potential stomatal resistance
      RSWP = VPSL*RPAR*(RBH+RT)
      RSWP = RSWP + VHCA*VPD
      RSWP =RSWP /(EP*LHVAP*PSYC)
      RSWP1 = VPSL/PSYC*(RBH+RT)
      RSWP1 = RSWP1+RBW+RT
      RSWP = abs(RSWP-RSWP1)
      !cALCULATE THE ACTUAL STOMATAL RESISTANCE
      EA = EP*PCEW
      RSWA = (VPSL*(RBH+RT)+PSYC*(RBW+RT))/(PSYC*EA)
      RSWA = (EP-EA)*RSWA
      RSWA = RSWA+RSWP*EP/EA      

END SUBROUTINE

SUBROUTINE FSWT(LAYERS, WCST, WCFC, WCWP, TKL,WCL, THRESHE,THRESHR,THRESHD,THRESHT, &
                        TROOTD,LESTRS, LRSTRS, LDSTRS, PCEW, OPTION)
!-------------------------------------------------------------------------------------------------------!
!This routine use FSWT function to quatify the reduction of water stress on leaf expension, leaf rolling!
!, leaf death rate and plant transpiration.                                                             !
!Developed by Dr. Tao Li, May 5, 2010                                                                   !
!-------------------------------------------------------------------------------------------------------!
!Variables      type      Array      description                                                                              unit        in/out!
!LAYERS     INTEGER N       Number of soil layers                                       -         in    !
!WCST            REAL      Y            Saturated water content                                                            cm3/cm3        in    !
!WCFC            REAL      Y            Soil field capacity                                                                  cm3/cm3        in    !
!WCWP            REAL      Y            Soil wilting point                                                                  cm3/cm3        in    !
!WCL            REAL      Y            Soil water content                                                                  cm3/cm3        in    !
!TKL            REAL    Y            Soil layer thickness                                                            m         in    !      
!THRESHE      REAL      N            Threshold value for leaf expansion reduction                        -         in    !
!THRESHR      REAL      N            Threshold value for leaf rolling reduction                              -         in    !
!THRESHD      REAL      N            Threshold value for leaf death reduction                              -         in    !
!THRESHT      REAL      N            Threshold value for transpiration reduction                              -         in    !
!LESTRS            REAL      N            Stress factor for leaf expansion                                          -         out   !
!TROOTD     REAL    N       Effective root depth                                        m         in    !
!LRSTRS            REAL      N            Stress factor for leaf rolling                                                -         out   !
!LDSTRS            REAL      N            Stress factor for leaf death                                                -         out   !
!PCEW            REAL      N            Stress factor for plant transpiration                                    -         IN/out!
!OPTION            INTEGER N       Option for calculate stress factor by FSWT function         -         in    !
!                                          1 = Option 1, calculated from PCEW, 0 = option 0, calculated from soil water!
!                                          hydraulic parameters                                                                                    !
!-------------------------------------------------------------------------------------------------------!
    IMPLICIT NONE
      REAL WCST(10), WCFC(10), WCWP(10), TKL(10),WCL(10)
      REAL THRESHE,THRESHR,THRESHD,THRESHT
      REAL LESTRS, LRSTRS, LDSTRS, PCEW, TROOTD
      INTEGER I, OPTION, LAYERS
      REAL ZZL, TZL, LE, LR, LD, LT, CRITIC
      IF(TROOTD.GT.0.0) THEN
            IF(OPTION.EQ.0) THEN
                  TZL = 0.0;ZZL = 0.0;LE=0.0;LR=0.0;LD=0.0;LT=0.0
                  DO I = 1, LAYERS
                        TZL=TZL + TKL(I)
                        CRITIC =MIN(1.0, MAX(0.0, (WCL(I)-WCWP(I))/(WCST(I)-WCWP(I))))
                        !FOR LEAF EXPANSION
                        IF(CRITIC.GE.THRESHE) THEN
                              IF(TZL.LE.TROOTD) THEN
                                    LE = LE + 1.0*TKL(I)/TROOTD
                              ELSE
                                    LE =LE + 1.0* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ELSE
                              IF(TZL.LE.TROOTD) THEN
                                    LE = LE + CRITIC/THRESHE*TKL(I)/TROOTD
                              ELSE
                                    LE =LE + CRITIC/THRESHE* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ENDIF
                        !FOR LEAF ROLLING
                        IF(CRITIC.GE.THRESHR) THEN
                              IF(TZL.LE.TROOTD) THEN
                                    LR = LR + 1.0*TKL(I)/TROOTD
                              ELSE
                                    LR =LR + 1.0* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ELSE
                              IF(TZL.LE.TROOTD) THEN
                                    LR = LR + CRITIC/THRESHR*TKL(I)/TROOTD
                              ELSE
                                    LR =LR + CRITIC/THRESHR* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ENDIF
                        !FOR LEAF DEATH
                        IF(CRITIC.GE.THRESHD) THEN
                              IF(TZL.LE.TROOTD) THEN
                                    LD = LD + 1.0*TKL(I)/TROOTD
                              ELSE
                                    LD =LD + 1.0* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ELSE
                              IF(TZL.LE.TROOTD) THEN
                                    LD = LD + CRITIC/THRESHD*TKL(I)/TROOTD
                              ELSE
                                    LD =LD + CRITIC/THRESHD* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ENDIF
                        !FOR TRANSPIRATION
                        IF(CRITIC.GE.THRESHT) THEN
                              IF(TZL.LE.TROOTD) THEN
                                    LT = LT + 1.0*TKL(I)/TROOTD
                              ELSE
                                    LT =LT + 1.0* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ELSE
                              IF(TZL.LE.TROOTD) THEN
                                    LT = LT + CRITIC/THRESHT*TKL(I)/TROOTD
                              ELSE
                                    LT =LT + CRITIC/THRESHT* (TROOTD-ZZL)/TROOTD
                              ENDIF
                        ENDIF 
                        ZZL = TZL                  
                  ENDDO
            ELSEIF(OPTION.EQ.1) THEN
                  IF(PCEW.GE.THRESHE) THEN
                        LE = 1.0
                  ELSE
                        LE = PCEW/THRESHE
                  ENDIF
                  IF(PCEW.GE.THRESHR) THEN
                        LR = 1.0
                  ELSE
                        LR = PCEW/THRESHR
                  ENDIF
                  IF(PCEW.GE.THRESHD) THEN
                        LD = 1.0
                  ELSE
                        LD = PCEW/THRESHD
                  ENDIF
                  IF(PCEW.GE.THRESHT) THEN
                        LT = 1.0
                  ELSE
                        LT = PCEW/THRESHT
                  ENDIF
            ENDIF
            LESTRS = LE
            LRSTRS = LR
            LDSTRS = LD
            PCEW   = LT
      ELSE
            LESTRS = 1.0
            LRSTRS = 1.0
            LDSTRS = 1.0
            PCEW   = 1.0
      ENDIF
END SUBROUTINE

