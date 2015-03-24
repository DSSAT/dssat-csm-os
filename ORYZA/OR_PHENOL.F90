!----------------------------------------------------------------------*
!  SUBROUTINE PHENOL                                                   *
!  Purpose: This subroutine calculates the rate of phenological        *
!           development of the crop based on photoperiod and           *
!           temperature.                                               *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! DVS     R4  Development stage of the crop (-)                     I  *
! DVRJ    R4  Development rate juvenile ((oCd)-1)                   I  *
! DVRI    R4  Development rate, photoperiod-sensitive phase         I  *
!             ((oCd)-1)                                                *
! DVRP    R4  Development rate, PI phase ((oCd)-1)                  I  *
! DVRR    R4  Development rate, reproductive phase ((oCd)-1)        I  *
! HU      R4  Heat units (oCd d-1)                                  I  *
! DAYL    R4  Astronomic daylength (base = 0 degrees) (h)           I  *
! MOPP    R4  Maximum optimum photoperiod (h)                       I  *
! PPSE    R4  Photoperiod sensitivity (h-1)                         I  *
! TS      R4  Temperature sum (oCd)                                 I  *
! SHCKD   R4  Delay parameter in phenology ((oCd)(oCd)-1)           I  *
! CROPSTA I4  Crop stage (-)                                        I  *
! DVR     R4  Development rate of the crop (d-1)                    O  *
! TSHCKD  R4  Transpl. shock for phenol. development (oCd)          O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE OR_PHENOL(ESTAB,DVS,DVRJ,DVRI,DVRP,DVRR,HU,DAYL,MOPP,PPSE, &
                        TS,SHCKD,CROPSTA,DVR,TSHCKD)

      IMPLICIT NONE
!-----Formal parameters
      REAL    DVS, DVRJ , DVRI, DVRP, DVRR, HU, DAYL, MOPP, PPSE
      REAL    TS , SHCKD, DVR , TSHCKD
	  CHARACTER (*) ESTAB
      INTEGER CROPSTA
!-----Local parameters
      REAL    DL, TSTR, PPFAC
      SAVE

      IF (DVS.GE.0..AND.DVS.LT.0.40) DVR = DVRJ*HU
      IF (DVS.GE.0.40.AND.DVS.LT.0.65) THEN
         DL = DAYL+0.9
         IF (DL.LT.MOPP) THEN
            PPFAC = 1.
         ELSE
            PPFAC = 1.-(DL-MOPP)*PPSE
         END IF
         PPFAC = MIN(1.,MAX(0.,PPFAC))
         DVR   = DVRI*HU*PPFAC
      
      END IF
      IF (DVS.GE.0.65.AND.DVS.LT.1.00) DVR = DVRP*HU 
      IF (DVS.GE.1.00)                 DVR = DVRR*HU
 
      IF (CROPSTA .EQ. 3) THEN
        TSTR = TS
      ELSEIF(CROPSTA .LT. 3) THEN
        TSTR = 0.0
      END IF            
      TSHCKD = SHCKD*TSTR
      IF (CROPSTA .GT. 3 .AND.TS.LT.(TSTR+TSHCKD)) DVR = 0.

      RETURN
      END
