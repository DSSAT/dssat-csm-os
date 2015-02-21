!----------------------------------------------------------------------*
!  SUBROUTINE SUBLAI3                                                  *
!  Version 2: January 2001                                             *
!          Version august, 503                                        *
!                                                                      *
!  Purpose: This subroutine calculates the rate of growth of LAI of    *
!    of the crop in the seedbed and after transplanting in the field.  *
!    Reductions by N-stress and water-stress are taken into account.   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! CROPSTA I4  Crop stage (-)                                        I  *
! RWLVG   R4  Green leaves growth rate (kg d-1 ha-1)                I  *
! DLDR    R4  Death rate green leaves (kg d-1 ha-1)                 I  *
! TSLV    R4  Temperature sum for leaf development (oC)             I  *
! HULV    R4  Daily temperature for leaf development (oC)           I  *
! SHCKL   R4  Delay parameter in development ((oCd)(oCd)-1)         I  *
! LESTRS  R4  Reduction factor for leaf elongation (-)              I  *
! SLA     R4  Specific leaf area (ha kg-1)                          I  *
! NH      R4  Number of hills (hills m-2)                           I  *
! NPLH    I4  Number of plants per hill (pl/hill)                   I  *
! NPLSB   R4  Number of plants in seedbed (pl/m2)                   I  *
! DVS     R4  Development stage of the crop (-)                     I  *
! LAI     R4  Leaf area index (ha ha-1)                             I  *
! RGRLMX  R4  Maximum relative growth rate leaves ((oCd)-1)         I  *
! RGRLMN  R4  Minimum relative growth rate leaves ((oCd)-1)         I  *
! ESTAB   C*  Establishment method (-)                              I  *
! GLAI    R4  Growth rate leaf area index (ha d-1 ha-1)             O  *
! RGRL    R4  Actual relative growth rate leaves ((oCd)-1)          O  *
!----------------------------------------------------------------------*
       SUBROUTINE SUBLAI3(CROPSTA,RGRLMX,RGRLMN,TSLV,HULV, &
                          SHCKL,LESTRS,RNSTRS,SLA,NH,NPLH,NPLSB,DVS,LAI, &
                          ESTAB,RWLVG,DLDR,WLVG,GLAI,RGRL)

      IMPLICIT NONE
!-----Formal parameters
      INTEGER       CROPSTA
      CHARACTER (*) ESTAB
      REAL          RGRL  , TSLV, HULV, SHCKL, NPLSB
      REAL          LESTRS, SLA  , NH  , NPLH, DVS , LAI  , GLAI
      REAL          RNSTRS, RGRLMX, RGRLMN, WLVG, RWLVG, DLDR
!-----Local parameters
      REAL          TSLVTR, TSHCKL, GLAI1,GLAI2, X, TESTSET
      REAL          WLVGEXP, LAIEXP, WLVGEXS, LAIEXS, TEST, DVSE
      LOGICAL       TESTL 

      REAL NOTNUL

      SAVE         ! TAOLI

      IF (CROPSTA .LE. 1) THEN
        X       = 1.
        TESTL   = .FALSE.
        TESTSET = 0.01
      END IF

!===================================================================*
!------Transplanted rice                                            *
!===================================================================*
!      Calculate RGRL as function of N stress limitation
       RGRL = RGRLMX - (1.-RNSTRS)*(RGRLMX-RGRLMN)

       IF (ESTAB .EQ. 'TRANSPLANT') THEN
!------- 1. Seed-bed; no drought stress effects in seed-bed!
         IF (CROPSTA .LT. 3) THEN
            IF (LAI.LT.1.) THEN
               GLAI = LESTRS * LAI*RGRL*HULV
               WLVGEXS = WLVG
               LAIEXS  = LAI
            ELSE
               IF (.NOT. TESTL) THEN
               TEST = ABS((LAI/NOTNUL(WLVG))-SLA)/SLA
                  IF (TEST .LT. TESTSET) TESTL = .TRUE.
               END IF
               IF (TESTL) THEN
                  GLAI = ((WLVG+RWLVG)*SLA)-LAI
               ELSE
                  GLAI1 = ((WLVG+RWLVG-WLVGEXS)*SLA+LAIEXS)-LAI
                  GLAI2 = ((WLVG+RWLVG)*SLA)-LAI
                  IF (GLAI2 .GT. 0.) THEN
                      GLAI  = (GLAI1+X*GLAI2)/(X+1.)
                  ELSE
                      GLAI = GLAI1
                  END IF
                  X     = X+1.
               END IF
            END IF
!------- 2. Transplanting effects: dilution and shock-setting
         ELSE IF (CROPSTA .EQ. 3) THEN
            TSLVTR = TSLV
            TSHCKL = SHCKL*TSLVTR
            GLAI   = (LAI*NH*NPLH/NPLSB) - LAI
            TESTL  = .FALSE.
            X      = 1.
!--------3. After transplanting: main crop growth
         ELSE IF (CROPSTA .EQ. 4) THEN
!--------3.1. During transplanting shock-period
            IF (TSLV.LT.(TSLVTR+TSHCKL)) THEN
               GLAI = 0.
               DVSE = DVS
!--------3.2. After transplanting shock; drought stress effects
            ELSE
               IF ((LAI.LT.1.0).AND.(DVS.LT.1.0)) THEN
                  GLAI = LESTRS * LAI*RGRL*HULV
                  WLVGEXP = WLVG
                  LAIEXP  = LAI
               ELSE
!                 There is a transition from RGRL to SLA determined growth
!                 when difference between simulated and imposed SLA is less than 1%
                  if(RWLVG.lt.0.0) then
                               TESTL =.true.   
                          elseif((RWLVG.ge.0.0).and.(testl)) then
                              TESTL =.false.
                          endif                  !Added by TaoLi, 10 Aug, 2010
                  IF (.NOT. TESTL) THEN
                     TEST = ABS((LAI/NOTNUL(WLVG))-SLA)/SLA
                     IF (TEST .LT. TESTSET) TESTL = .TRUE.
                  END IF
                  IF (TESTL) THEN
                     GLAI = ((WLVG+RWLVG-DLDR)*SLA)-LAI
                  ELSE
                     GLAI1 = ((WLVG+RWLVG-DLDR-WLVGEXP)*SLA+LAIEXP)-LAI
                     GLAI2 = ((WLVG+RWLVG-DLDR)*SLA)-LAI
                     IF (GLAI2 .LT. 0. .AND. GLAI1 .GT. 0.) THEN
                        GLAI = GLAI1/(X+1)
                     ELSE
                        GLAI  = (GLAI1+X*GLAI2)/(X+1.)
                     END IF
                     X     = X+1.
                  END IF
               END IF
            END IF
         END IF
 
!===================================================================*
!------Direct-seeded rice                                           *
!===================================================================*
       ELSE IF (ESTAB .EQ. 'DIRECT-SEED') THEN
         IF ((LAI.LT.1.0).AND.(DVS.LT.1.0)) THEN
            GLAI    = LAI*RGRL*HULV * LESTRS
            WLVGEXP = WLVG
            LAIEXP  = LAI
         ELSE
!           There is a transition from RGRL to SLA determined growth
!           when difference between simulated and imposed SLA is less than 10%
                  if(RWLVG.lt.0.0) then
                        TESTL =.true.   
                  elseif((RWLVG.ge.0.0).and.(testl)) then
                        TESTL =.false.
                  endif                  !Added by TaoLi, 10 Aug, 2010
            IF (.NOT. TESTL) THEN
               TEST = ABS((LAI/NOTNUL(WLVG))-SLA)/SLA
               IF (TEST .LT. TESTSET) TESTL = .TRUE.
            END IF
            IF (TESTL) THEN
               GLAI = ((WLVG+RWLVG-DLDR)*SLA)-LAI
            ELSE
               GLAI1 = ((WLVG+RWLVG-DLDR-WLVGEXP)*SLA+LAIEXP)-LAI
               GLAI2 = ((WLVG+RWLVG-DLDR)*SLA)-LAI
               IF (GLAI2 .LT. 0. .AND. GLAI1 .GT. 0.) THEN
                  GLAI = GLAI1/(X+1)
               ELSE
                  GLAI  = (GLAI1+X*GLAI2)/(X+1.)
               END IF
               X     = X+1.
            END IF
         END IF
       END IF
 
      RETURN
      END
