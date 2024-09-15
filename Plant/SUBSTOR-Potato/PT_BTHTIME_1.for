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

      SUBROUTINE PT_BTHTIME_1 (
     &    ISTAGE, TMAX, TMIN, TBD, TOD, TCD, !Input, removed DIF, DAYL
     &    TSEN, SBD, SOD, SCD, SSEN,         !Input
     &    TDU_1, SDU_1)                      !Output
      
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for
      
      IMPLICIT NONE
      SAVE
      !INTEGER DS
      INTEGER ISTAGE, I                                   
      REAL TMAX, TMIN, DAYL, TBD, TOD, TCD, TSEN, TDU_1, SDU_1   !removed TDU, SDU, DIF, DAYL
      REAL SBD, SOD, SCD, SSEN
      REAL TT, ST, SU, TU, TMEAN !removed XTEMP, SS, TD, SD, SUNRIS, SUNSET,
      
      ! To test whether model is working with repect to temperture
      !TMAX = 0.0
      !TMIN = -10.0
      !TMAX = 50.0
      !TMIN = 37.0
      
! Mathematical equation to fortran variables mapping:
! TCD is Tc, TD is T, TOD is To, TBD is Tb, and TSEN is ct. 
! Constants Tb = 5.5 ˚C, To= 23.4 ˚C, Tc= 34.6 ˚C, ct = 1.6
! i.e. TBD=5.5, TOD=23.4, TCD=34.6, TSEN=1.6, respectively
! As TBD, TOD, TCD and TSEN are declared as input variables,
! these constant values must be provided at the call site.

! Disabled line numbers73-74
!*---timing for sunrise and sunset
!      SUNRIS = 12.0 - 0.5 * DAYL
!      SUNSET = 12.0 + 0.5 * DAYL

!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      !XTEMP = (TMAX + TMIN)/2.0
      TT     = 0.0
      !SS     = 0.0
      ST     = 0.0
      TDU_1  = 0.0
      SDU_1  = 0.0
    
! Disabled line numbers 87-102
!*---diurnal course of temperature
!      DO 10 I = 1, 24
!        IF (I.GE.SUNRIS .AND. I.LE.SUNSET) THEN
          !TD = XTEMP+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          TD = TMEAN + DIF + 0.5 * ABS(TMAX - TMIN) * 
!     &    COS(0.2618 * FLOAT(I - 14)) 
          !SD = XTEMP+DIF+0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          SD = TMEAN + DIF + 0.5 * ABS(TMAX - TMIN) * 
!     &    COS(0.2618 * FLOAT(I - 14))  
!        ELSE
          !TD = XTEMP    +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          TD = TMEAN + 0.5 * ABS(TMAX - TMIN) * 
!     &    COS(0.2618 * (I - 14)) 
          !SD = XTEMP    +0.5*ABS(TMAX-TMIN)*COS(0.2618*FLOAT(I-14))
!          SD = TMEAN + 0.5 * ABS(TMAX - TMIN) * 
!     &    COS(0.2618 * (I - 14)) 
!        ENDIF
           
! Disabled line numbers 107-114 (It was giving errors in DTT and STT output)
!*---assuming development rate at supra-optimum (above optimum) temperatures during
!*   the reproductive phase equals that at the optimum temperature
        ! IF (DS.GT.1.) THEN
       ! IF (ISTAGE.EQ.2) THEN
       !   TD = MIN (TD,TOD)
       !   SD = MIN (SD,SOD)
       ! ELSE
       !    TD = TD
       !    SD = SD
       ! ENDIF

!*---instantaneous thermal unit based on bell-shaped temperature response
!        IF (TD.LT.TBD .OR. TD.GT.TCD) THEN
!          TU = 0.0
       IF (TMEAN.LT.TBD .OR. TMEAN.GT.TCD) THEN
           TU = 0.0
!        ELSE IF (TD .GE. TBD .AND. TD .LE. TCD) THEN
!           TU = (((TCD-TD)/(TCD-TOD))*((TD-TBD)/(TOD-TBD))**
!    &          ((TOD-TBD)/(TCD-TOD)))**TSEN
        ELSE IF (TMEAN .GE. TBD .AND. TMEAN .LE. TCD) THEN
           TU = (((TCD-TMEAN)/(TCD-TOD))*((TMEAN-TBD)/(TOD-TBD))**
     &          ((TOD-TBD)/(TCD-TOD)))**TSEN
           
        ENDIF

!        IF (SD.LT.SBD .OR. SD.GT.SCD) THEN
!           SU = 0.0
       IF (TMEAN.LT.SBD .OR. TMEAN.GT.SCD) THEN
           SU = 0.0
!        ELSE IF (SD .GE. SBD .AND. SD .LE. SCD) THEN
!           SU = (((SCD-SD)/(SCD-SOD))*((SD-SBD)/(SOD-SBD))**
!     &          ((SOD-SBD)/(SCD-SOD)))**SSEN
        ELSE IF (TMEAN .GE. SBD .AND. TMEAN .LE. SCD) THEN
           SU = (((SCD-TMEAN)/(SCD-SOD))*((TMEAN-SBD)/(SOD-SBD))**
     &          ((SOD-SBD)/(SCD-SOD)))**SSEN
            
        ENDIF

        !TT = TT + TU/24.0
        TT = TT + TU
        !ST = ST + SU/24.0
        ST = ST + SU
        !SS = SS + SU/24.0

!*---effect of instantaneous temperature on maintenance respiration
 ! 10  CONTINUE

!*---daily thermal unit for phenology
      IF (ISTAGE .LE. 4) THEN
        TDU_1  = TT
      END IF
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
        !SDU  = SS
        SDU_1  = ST
        
      END IF

!*---daily average of temperature effect on maintenance respiration
        
      RETURN
      END SUBROUTINE PT_BTHTIME_1
