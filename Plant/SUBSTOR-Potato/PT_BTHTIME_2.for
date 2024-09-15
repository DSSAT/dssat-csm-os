!*----------------------------------------------------------------------*
!*  SUBROUTINE PT_BTHTIME                                               *
!*  Purpose: This subroutine calculates the daily amount of thermal day *
!*           and the average effect of daily temperature on maintenance *
!*           respiration                                                *
!*                                                                      *
!*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)     *
!*                                                                      *
!*  name   type meaning                                    units  class *
!*  ----   ---- -------                                    -----  ----- *
!*  DS      R4  Development stage                            -       I  *
!*  TMAX    R4  Daily maximum temperature                    oC      I  *
!*  TMIN    R4  Daily minimum temperature                    oC      I  *
!*  DIF     R4  Daytime plant-air temperature differential   oC      I  *
!*  DAYL    R4  Astronomic daylength (base = 0 degrees)      h       I  *
!*  TBD     R4  Base air temperature for phenology           oC      I  *
!*  SBD     R4  Base soil temperature for phenology          oC      I  *
!*  TOD     R4  Optimum air temperature for phenology        oC      I  *
!*  SOD     R4  Optimum soil temperature for phenology       oC      I  *
!*  TCD     R4  Ceiling air temperature for phenology        oC      I  *
!*  SCD     R4  Ceiling soil temperature for phenology       oC      I  *     
!*  TSEN    R4  Curvature for air temperature response       -       I  *
!*  SSEN    R4  Curvature for soil temperature response      -       O  *
!*  TDU     R4  Daily thermal-day unit Air                   -       O  *
!*  SDU     R4  Daily thermal-day unit Soil                  -       O  *      
!*----------------------------------------------------------------------*
C=======================================================================
C  PT_BTHTIME, Subroutine
C
C  Determines Beta thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  10/30/2023 MSKhan added Beta thermal time sub-routine following Yin et al., 2003 
C  
C=======================================================================

      SUBROUTINE PT_BTHTIME_2 (
     &    ISTAGE, L0, ST, TMAX, TMIN, TBD, TOD, TCD,   !Input, removed DIF, DAYL
     &    TSEN, SBD, SOD, SCD, SSEN,         !Input
     &    TDU_2, SDU_2)                                     !Output TDU, SDU
      
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for
      
      IMPLICIT NONE
      !INTEGER DS
      INTEGER ISTAGE, L0, I
      REAL TMAX, TMIN, TBD, TOD, TCD, TSEN, TDU_2, SDU_2 !removed DIF, DAYL
      REAL SBD, SOD, SCD, SSEN
      REAL TT, TU, SS, SU, TMEAN  !removed XTEMP, TD, SD, SUNRIS, SUNSET
      REAL ST(NL)
      SAVE
      ! To test whether model is working with repect to temperture
      !TMAX = 50.0
      !TMIN = 37.0
      !TMAX = 0.0
      !TMIN = -10.0
      !ST(L0) = 55.0
      

! Mathematical equation to fortran variables mapping:
! TCD is Tc, TD is T, TOD is To, TBD is Tb, and TSEN is ct. 
! Constants Tb = 5.5 ˚C, To= 23.4 ˚C, Tc= 34.6 ˚C, ct = 1.6
! i.e. TBD=5.5, TOD=23.4, TCD=34.6, TSEN=1.6, respectively
! As TBD, TOD, TCD and TSEN are declared as input variables,
! these constant values must be provided at the call site.

! Disabled line numbers73-76
!*---timing for sunrise and sunset
!     SUNRIS = 12.0 - 0.5*DAYL
!     SUNSET = 12.0 + 0.5*DAYL

!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      !XTEMP  = (TMAX + TMIN)/2.0
      TT     = 0.0
      SS     = 0.0
      TDU_2  = 0.0
      SDU_2  = 0.0

      !PRINT 101, 'TMIN', TMIN, 'TMAX', TMAX, 'ST(L0)', ST(L0)
 !101  FORMAT(A5, F6.2, A5, F6.2, A7, F6.2)
! Disabled line numbers 87-102
!*---diurnal course of temperature
!      DO 10 I = 1, 24
!        IF (I.GE.SUNRIS .AND. I.LE.SUNSET) THEN
!          !TD = XTEMP+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          TD = (TMAX + TMIN) / 2.0 + 
!     &    DIF + 0.5 * ABS(TMAX - TMIN) * COS(0.2618 * FLOAT(I - 14))
!          SD = ST(L0)+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!        ELSE
!          !TD = XTEMP    +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          TD = (TMAX + TMIN) / 2.0 + 
!     &    0.5 * ABS(TMAX - TMIN) * COS(0.2618 * FLOAT(I - 14))
!          SD = ST(L0)   +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!        ENDIF

        !PRINT 102, 'HR', I, 'SD', SD
 !102    FORMAT(A3, I2, A3, F6.2)
        
!*---assuming development rate at supra-optimum (above optimum) temperatures during
!*   the reproductive phase equals that at the optimum temperature
        ! IF (DS.GT.1.) THEN
!        IF (ISTAGE.EQ.2) THEN
!           TD = MIN (TD,TOD)
!           SD = MIN (SD,SOD)
!        ELSE
!           TD = TD
!           SD = SD
!        ENDIF

!*---instantaneous thermal unit based on bell-shaped temperature response
        IF (TMEAN.LT.TBD .OR. TMEAN.GT.TCD) THEN
           TU = 0.0
        ELSE
           TU = (((TCD-TMEAN)/(TCD-TOD))*((TMEAN-TBD)/(TOD-TBD))**
     &          ((TOD-TBD)/(TCD-TOD)))**TSEN
        ENDIF

        IF (ST(L0).LT.SBD .OR. ST(L0).GT.SCD) THEN
           SU = 0.0
        ELSE
           SU = (((SCD-ST(L0))/(SCD-SOD))*((ST(L0)-SBD)/(SOD-SBD))**
     &          ((SOD-SBD)/(SCD-SOD)))**SSEN
        ENDIF

!       TT = TT + TU/24.0
        TT = TT + TU
!        SS = SS + SU/24.0
        SS = SS + SU

  !10  CONTINUE

!*---daily thermal unit for phenology
      IF (ISTAGE .LE. 4) THEN
        TDU_2  = TT
      END IF
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
        SDU_2  = SS
      END IF
        
      RETURN
      END SUBROUTINE PT_BTHTIME_2
