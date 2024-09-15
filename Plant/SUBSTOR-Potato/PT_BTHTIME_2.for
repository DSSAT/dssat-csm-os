C=======================================================================
C  PT_BTHTIME_2, Subroutine
C
C  Determines Beta thermal time calculations
C-----------------------------------------------------------------------
C  Revision history
C
C               Written
C  10/30/2023 MSKhan added Beta thermal time sub-routine following 
C  Yin et al., 2003 
C  
C=======================================================================

      SUBROUTINE PT_BTHTIME_2 (
     &    ISTAGE, L0, ST, TMAX, TMIN, TBD, TOD, TCD,   !Input
     &    TSEN, SBD, SOD, SCD, SSEN,         !Input
     &    TDU_2, SDU_2)                      !Output 
      
!     ------------------------------------------------------------------
      USE ModuleDefs     !Definitions of constructed variable types, 
                         ! which contain control information, soil
                         ! parameters, hourly weather data.
!     NL defined in ModuleDefs.for
      
      IMPLICIT NONE
      INTEGER ISTAGE, L0, I
      REAL TMAX, TMIN, TBD, TOD, TCD, TSEN, TDU_2, SDU_2 
      REAL SBD, SOD, SCD, SSEN
      REAL TT, TU, SS, SU, TMEAN  
      REAL ST(NL)
      SAVE
      ! To test whether model is working with repect to temperture
      !TMAX = 50.0
      !TMIN = 37.0
      !TMAX = 0.0
      !TMIN = -10.0
      !ST(L0) = 55.0
      
!*---mean daily temperature
      TMEAN  = (TMAX + TMIN)/2.0
      TT     = 0.0
      SS     = 0.0
      TDU_2  = 0.0
      SDU_2  = 0.0

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

        TT = TT + TU
        SS = SS + SU

!*---daily thermal unit for phenology
      IF (ISTAGE .LE. 4) THEN
        TDU_2  = TT
      END IF
      IF (ISTAGE .GE. 6 .OR. ISTAGE .LE. 2) THEN
        SDU_2  = SS
      END IF
        
      RETURN
      END SUBROUTINE PT_BTHTIME_2
