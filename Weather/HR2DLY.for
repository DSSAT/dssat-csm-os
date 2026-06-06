!=================================================================
!  HR2DLY.for, Subroutine, Fabio Oliveira
!  Calculates daily values of temperature, solar radiation,
!  and rain from hourly input file with FlexibleIO
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  10/06/2025  FO Written
!=======================================================================

      SUBROUTINE HR2DLY(YRDOY,                             !Input
     &    SRAD, TMAX, TMIN, RAIN)                          !Output

!-----------------------------------------------------------------------
      USE flexibleio
      IMPLICIT NONE
      INTEGER H,TINCR, YRDOY

      ! Daily (D) and (H) weather variables
      REAL HSRAD, SRADMJ, SRADJ, HTMAX, HTMIN, HRAIN
      REAL DSRAD, DTMAX, DTMIN, DRAIN
      
      ! Weather variables converted
      REAL SRAD, TMAX, TMIN, RAIN
      
      PARAMETER (TINCR=24)
!-----------------------------------------------------------------------
!     Initialize
      DSRAD = 0.0
      DRAIN = 0.0
      
!     Loop to compute daily weather data over hourly data.
      DO H = 1, TINCR
        CALL fio % get('WTH', YRDOY, H, 'SRAD',HSRAD)
        CALL fio % get('WTH', YRDOY, H, 'TMAX',HTMAX)
        CALL fio % get('WTH', YRDOY, H, 'TMIN',HTMIN)
        CALL fio % get('WTH', YRDOY, H, 'RAIN',HRAIN)
        
        !Solar Radiation converstion from W/m2/hour to MJ/m2/hour
        SRADMJ = HSRAD * 0.0036
        !J/m2/s the same unit as W m^-2
        SRADJ  = HSRAD
        CALL fio % set('WTH', YRDOY, H, 'SRADJ',SRADJ)

        DSRAD = DSRAD + SRADMJ
        DRAIN = DRAIN + HRAIN
        IF(H .EQ. 1) THEN
            DTMAX = HTMAX
            DTMIN = HTMIN
        ELSE
            DTMAX = MAX(DTMAX,HTMAX)
            DTMIN = MIN(DTMIN,HTMIN)
        ENDIF
      ENDDO
      
      !Assign the values converted
      SRAD = DSRAD
      TMAX = DTMAX
      TMIN = DTMIN
      RAIN = DRAIN
      
      RETURN
      END SUBROUTINE HR2DLY