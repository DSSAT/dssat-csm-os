!----------------------------------------------------------------------*
!  SUBROUTINE SUBDD                                                    *
!  Purpose: This subroutine calculates the daily amount of heat units  *
!           for calculation of the phenological development rate and   *
!           early leaf area growth.                                    *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! TMIN    R4  Daily minimum temperature (oC)                        I  *
! TBD     R4  Base temperature for development (oC)                 I  *
! TOD     R4  Optimum temperature for development (oC)              I  *
! TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (oCd d-1)                                  O  *
!                                                                      *
!  FILE usage : none                                                   *
!----------------------------------------------------------------------*
      SUBROUTINE SUBDD(TMAX,TMIN,TBD,TOD,TMD,HU)

      IMPLICIT NONE
!-----Formal parameters
      REAL    TMAX,TMIN,TBD,TOD,TMD,HU
!-----Local parameters
      REAL    TD, TM, TT, X1, X2
      INTEGER I
      SAVE         ! TAOLI
 
      TM = (TMAX+TMIN)/2.
      TT = 0.
	  X1= (TOD-TBD)/(TMD-TOD)		!TAOLI, APRIL 1,2009
	  X2=0.0						!TAOLI, APRIL 1,2009					
      DO I = 1,24
         TD = TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		 
         IF ((TD.GT.TBD).AND.(TD.LT.TMD)) THEN

            IF (TD.GT.TOD) TD = TOD-(TD-TOD)*(TOD-TBD)/(TMD-TOD) !TAOLI, APRIL 1,2009, COMMENT OUT
            TT = TT+(TD-TBD)/24.
         END IF
 
      END DO
      HU = TT
 
      RETURN
      END
