
!----------------------------------------------------------------------*
!  SUBROUTINE TSHIFT                                                   *
!  Purpose: This subroutine calculates the shift of daily average	   *
!			temperature and the change of heat unit due to air		   *
!           temperature is controlled in a period of a day			   *
!           Author: Tao Li, IRRI, June 5, 509.						   *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning (unit)                                     class *
! ----   ---- ---------------                                    ----- *
! TMAX    R4  Daily maximum temperature (oC)                        I  *
! TMIN    R4  Daily minimum temperature (oC)                        I  *
! DOYC    R4  The day in a year at temperature is controlled        I  *
! SHOUR   R4  The starting hour of temperature control (24 base)    I  *
! EHOUR   R4  The ending hour of temperature control				I  *
! TTEMP	  R4  The target controlled temperature                     I  *
! TCHANG  R4  The net change of temperature during control          I  *	
! TBD     R4  Base temperature for development (oC)                 I  *
! TOD     R4  Optimum temperature for development (oC)              I  *
! TMD     R4  Maximum temperature for development (oC)              I  *
! HU      R4  Heat units (oCd d-1)                                  O  *
! xTAV	  R4  Shifted amount of average temperature					O  *
! xTMIN   R4  Shifted amount of minimum temperature                 O  *
! xTMAX   R4  Shifted amount of maximum temperature                 O  * 
! xHU     R4  Shifted amount of thermal unit                        O  *
!                                                                      *
!  FILE usage : none                                                   *
! If TTEMP is a number larger than -999, then TCHANG must be set -999, *
!  or in reverse way. Only one number is used to calculate the shifting*
!----------------------------------------------------------------------*
      SUBROUTINE TSHIFT(TMAX,TMIN,DAYL, SHOUR, EHOUR, TTEMP, TCHANG, &
		ISTEMPC, CONTRM, TBD,TOD,TMD,HU2, xTAV, xTMIN, xTMAX, xHU, xTAVD)

      IMPLICIT NONE
!-----Formal parameters
      REAL    TMAX,TMIN,TBD,TOD,TMD,HU2, xHU
	  REAL DAYL, SHOUR, EHOUR, TTEMP, TCHANG, xTAV, xTMIN, xTMAX, xTAVD
!-----Local parameters
      REAL    TD, TM, TT, DST, DET, TMPV !, DAVERT
	  REAL   MYT(24)  !,MYTEM
      INTEGER I, ISTEMPC, CONTRM
      SAVE         !&#@TAOLI
!Calculate hourly temperature
	  TM = (TMAX+TMIN)/2.
      DO I = 1,24
		 MYT(I) = 0.0
         TD = TM+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
		 MYT(I) = TD
		 IF ((TD.GT.TBD).AND.(TD.LT.TMD)) THEN
            IF (TD.GT.TOD) TD = TOD-(TD-TOD)*(TOD-TBD)/(TMD-TOD)
            TT = TT+(TD-TBD)/24.
         END IF
      END DO
	  HU2=TT
!Impose the temperature variation into target period
	  DO I = 1, 24
		 IF(ISTEMPC.EQ.2) THEN
			IF((I.GE.SHOUR).AND.(I.LE.EHOUR)) THEN
				IF((TTEMP.NE.-999.0).AND.(TCHANG.EQ.-999.0)) THEN
					IF(CONTRM.EQ.1) THEN
						IF(MYT(I).LT.TTEMP) MYT(I) = TTEMP
					ELSEIF(CONTRM.EQ.2) THEN
						MYT(I) = TTEMP
					ENDIF
				ELSEIF((TTEMP.EQ.-999.0).AND.(TCHANG.NE.-999.0)) THEN
					MYT(I) = MYT(I) + TCHANG
				ENDIF		
			ENDIF
		ELSEIF(ISTEMPC.EQ.1) THEN
			IF(((I.GE.0).AND.(I.LE.EHOUR)).OR.((I.GE.SHOUR).AND.(I.LE.24))) THEN
				IF((TTEMP.NE.-999.0).AND.(TCHANG.EQ.-999.0)) THEN
					IF(CONTRM.EQ.1) THEN
						IF(MYT(I).LT.TTEMP) MYT(I) = TTEMP
					ELSEIF(CONTRM.EQ.2) THEN
						MYT(I) = TTEMP
					ENDIF
				ELSEIF((TTEMP.EQ.-999.0).AND.(TCHANG.NE.-999.0)) THEN
					MYT(I) = MYT(I) + TCHANG
				ENDIF
			ENDIF
		ENDIF
	  ENDDO
!Check whether the max and min temperature changed, and calculate the average temperature
	  XTAV=0.0; XTMIN = TMIN; XTMAX = TMAX
	  DO I = 1, 24
		 IF(MYT(I).GT.TMAX) XTMAX = MYT(I)
		 IF(MYT(I).LT.TMIN) XTMIN = MYT(I)
		 XTAV = XTAV + MYT(I)
	  ENDDO
	  XTAV = XTAV/24.0 - TM
	  XTMIN = XTMIN - TMIN
	  XTMAX = XTMAX - TMAX

!Calculate the thermal unit
      TT = 0.
      DO I = 1,24
         TD = MYT(I)
         IF ((TD.GT.TBD).AND.(TD.LT.TMD)) THEN
            IF (TD.GT.TOD) TD = TOD-(TD-TOD)*(TOD-TBD)/(TMD-TOD)
            TT = TT+(TD-TBD)/24.
         END IF
 
      END DO
      xHU = TT - HU2
!CALCULATE THE ACTUAL DAY TIME AVERAGE
	  DST = 12.0 - DAYL/2.0; DET = 12.0 + DAYL/2.0; XTAVD = 0.0
	  DO I = 1, 24
		 IF((I.GE.DST).AND.(I.LE.DET)) THEN
			XTAVD = XTAVD + MYT(I)
		 ENDIF
      ENDDO
	  XTAVD = XTAVD/(DET-DST)
	  TMPV  = (TMIN+TMAX)/2.
      TMPV = (TMAX+TMPV )/2.
	  XTAVD = XTAVD - TMPV
 
      RETURN
      END

