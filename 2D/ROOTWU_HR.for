!=======================================================================
C  ROOTWU_HR, Subroutine, J.T. Ritchie
C  Calculates root water uptake rate for each soil layer and total rate.
!  Routine is called daily, but computes on an hourly basis.
C-----------------------------------------------------------------------
C  REVISION       HISTORY
C  01/01/1989 JR  Written
C  12/05/1993 NBP Made into subroutine.
C  01/18/1996 JWJ Added flooding effect on water uptake
C  01/06/1996 GH  Added soil water excess stress
C  10/10/1997 CHP Updated for modular format.
C  09/01/1999 GH  Incorporated in CROPGRO
C  01/10/2000 NBP Added SAVE for stored variables and set SWCON2=RWU=0.0
C  01/12/2000 NBP Removed FILECC from input
C  01/25/2000 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
C  06/21/2001 GH  Added seasonal initialiation
C  09/17/2001 CHP Input PORMIN and RWUMX from Plant module.
!  08/10/2009 CHP Converted 1D routine to hourly.
C                   
C-----------------------------------------------------------------------
C Called by: SPAM
C Calls:     None
C=======================================================================
      SUBROUTINE ROOTWU_HR(DYNAMIC,  
     &    DLAYR, EOP, ES_LYR, LL, NLAYR, RLV,             !Input
     &    SAT, SW, WEATHER,                               !Input
     &    EP, RWU, SWDELTX, SWFAC, TRWU, TRWUP, TURFAC)   !Output

C-----------------------------------------------------------------------
      USE ModuleDefs
      USE ModuleData
      IMPLICIT NONE
      SAVE
C-----------------------------------------------------------------------
      INTEGER DYNAMIC, I, L, LUN2, NLAYR, N_HRS
      INTEGER TSS(NL)

      REAL Scale2Hour, EXPONENT
      REAL EOP, EP, PORMIN, RWU_soil_lim, RWU_root_lim, RWUEP1, RWUMX
      REAL SRAD_TOT, SWCON1, SWCON3, SWEXF, TRWU, TRWUP, WUF
      REAL, DIMENSION(NL) :: DLAYR, ES_LYR, LL, RLV, RWU
      REAL, DIMENSION(NL) :: SAT, SW, SW_AVAIL, SWCON2, SWDELTX, SWTEMP
      REAL, DIMENSION(24) :: EOP_HR, TRWU_HR, TRWUP_hr
      REAL, DIMENSION(24,NL) :: RWU_HR, RWUP_HR

      REAL SWFAC, SWFAC_HR, TURFAC, TURFAC_HR
      
      TYPE (WeatherType) WEATHER

      PARAMETER (SWCON1 = 1.32E-3)
      PARAMETER (SWCON3 = 7.01)

!!     temp chp
!      type (controltype) CONTROL
!      CALL GET(CONTROL)

!***********************************************************************
!***********************************************************************
!     Seasonal Initialization - Called once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
C     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
C     high LL to avoid water uptake limitations.
!-----------------------------------------------------------------------
      EOP_HR = 0.0
      RWU    = 0.0           
      RWU_HR = 0.0
      RWUP_HR = 0.0
      SWCON2 = 0.0        
      SWDELTX = 0.0
      TSS    = 0.0
      TRWUP  = 0.0
      TRWUP_hr = 0.0

      write(*,'(/," Start 1D hourly model")')

!      CALL GETLUN('RWU_1D.CSV',LUN2)
!      OPEN (UNIT=LUN2, FILE='RWU_1D.CSV')
!      WRITE(LUN2,'(A)') "1D, hourly root water uptake"
!      WRITE(LUN2,'(A)') "TIME(d), EOP, TRWUP, TRWU, SWFAC, TURFAC"

      CALL GET('PLANT', 'RWUMX',  RWUMX)
      CALL GET('PLANT', 'PORMIN', PORMIN)
      CALL GET('PLANT', 'RWUEP1', RWUEP1)

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
C-----------------------------------------------------------------------
      RWU    = 0.0
      TRWU   = 0.0
      TRWUP  = 0.0
      EOP_HR = 0.0
      RWU_HR = 0.0
      RWUP_HR= 0.0
      TRWU_HR= 0.0
      TRWUP_HR=0.0
      SWDELTX= 0.0
      SWTEMP = SW

!      Scale2Hour = 24. / WEATHER % DAYL

      IF (EOP < 1.E-7) THEN
        SWFAC  = 1.0
        TURFAC = 1.0
        RETURN
      ENDIF

      SWFAC  = 0.0
      TURFAC = 0.0

      DO L = 1,NLAYR
        SWCON2(L) = 120. - 250. * LL(L)
        IF (LL(L) .GT. 0.30) SWCON2(L) = 45.0
        SW_AVAIL(L) = SW(L) - 0.1 * ES_LYR(L) / DLAYR(L) - LL(L)
        SW_AVAIL(L) = MAX(0.0, SW_AVAIL(L))
      ENDDO

      SRAD_TOT = SUM(WEATHER%RADHR)
      N_HRS = 0     !# of daylight hours

      HourLoop: DO I = 1, 24

!       EOP_HR in mm/h
        EOP_HR(I) = EOP * WEATHER%RADHR(I) / SRAD_TOT
        IF (EOP_HR(I) < 1.E-9) THEN
          TRWUP_hr(I) = 0.0
          TRWU_HR(I)  = 0.0
          DO L = 1, NLAYR
            RWU_HR(I,L) = 0.0
          ENDDO
          CYCLE
        ENDIF

        LayerLoop: DO L = 1,NLAYR
          IF (RLV(L) .LE. 0.00001 .OR. SW(L) .LE. LL(L)) THEN
            RWUP_HR(I,L) = 0.
          ELSE
!           RWU in cm3[water]/cm[root]-d
            EXPONENT = MIN((SWCON2(L) * (SW(L) - LL(L))), 40.)
            RWU_soil_lim = SWCON1 * EXP(EXPONENT)/
     &        (SWCON3-ALOG(RLV(L))) / 24.

            IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
               TSS(L) = 0
            ELSE
               TSS(L) = TSS(L) + 1
            ENDIF
!             Delay of 2 days after soil layer is saturated before root
!             water uptake is affected
            IF (TSS(L) .GT. 48) THEN
               SWEXF = (SAT(L)-SW(L))/PORMIN
               SWEXF = MAX(SWEXF,0.0)
            ELSE
               SWEXF = 1.0
            ENDIF
            SWEXF = MIN(SWEXF,1.0)
            RWU_root_lim = RWUMX * SWEXF * Scale2Hour / 24.
            RWUP_hr(I,L) = MIN(RWU_soil_lim, RWU_root_lim)
!            !temp chp
!            if (control.das == 68) then
!              write(lun2,'(I4,2(",",I4),3(",",F10.4))') control.das, 
!     &          i, L, RWU_soil_lim, RWU_root_lim, RWUP_hr(I,L)
!            endif
          ENDIF
  
          RWUP_hr(I,L) = RWUP_hr(I,L) * DLAYR(L) * RLV(L) 
!          cm[water]    cm3[water]    cm3[soil]   cm[root] 
!          ---------  = ----------  * --------- * ---------
!             hr        cm[root]-hr   cm2[soil]   cm3[soil]
  
          TRWUP_hr(I)  = TRWUP_hr(I) + RWUP_hr(I,L)     !cm/hr
        ENDDO LayerLoop

        TRWUP = TRWUP + TRWUP_hr(I)
        N_HRS = N_HRS + 1
      
        CALL WaterStress(EOP_hr(I), RWUEP1, TRWUP_hr(I), 
     &      SWFAC_hr, TURFAC_hr)

        SWFAC  = SWFAC  + SWFAC_HR
        TURFAC = TURFAC + TURFAC_HR

!-----------------------------------------------------------------------
!       From EXTRACT
        IF ((0.1 * EOP_HR(I)) .LE. TRWUP_HR(I)) THEN
          WUF = 0.1 * EOP_HR(I) / TRWUP_HR(I)
        ELSE
          WUF = 1.0
        ENDIF
        
        TRWU_HR(I) = 0.0
        DO L = 1, NLAYR
          IF (SWTEMP(L) .GT. LL(L)) THEN
            RWU_HR(I,L) = RWUP_HR(I,L) * WUF
            IF (RWU_HR(I,L) / DLAYR(L) .GT. SW_AVAIL(L)) THEN
              RWU_HR(I,L) = SW_AVAIL(L) * DLAYR(L)
            ENDIF
          ENDIF
          RWU(L) = RWU(L) + RWU_HR(I,L)
          TRWU_HR(I) = TRWU_HR(I) + RWU_HR(I,L)
        
          SWTEMP(L) = SWTEMP(L) - RWU_HR(I,L) / DLAYR(L)
!         Available water for next time step
          SW_AVAIL(L) = SWTEMP(L) - LL(L)
        ENDDO
        
        TRWU = TRWU + TRWU_HR(I)    !cm

!!       TEMP CHP
!!        IF (CONTROL.DAS == 68) THEN
!          WRITE(LUN2,'(F10.3,19(",",F10.4))') CONTROL%DAS+float(i)/24.,
!     &     EOP_HR(I), TRWUP_HR(I)*10., TRWU_HR(I)*10., 
!     &     SWFAC_HR, TURFAC_HR
!!        ENDIF

      ENDDO HourLoop

      SWFAC  = SWFAC  / N_HRS
      TURFAC = TURFAC / N_HRS

      EP = TRWU * 10.       !EP in mm, TRWU in cm
      DO L = 1, NLAYR
        SWDELTX(L) = SWTEMP(L) - SW(L)
      ENDDO

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!***********************************************************************
      RETURN
      END SUBROUTINE ROOTWU_HR

!-----------------------------------------------------------------------
!     ROOTWU VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! DLAYR(L)  Soil thickness in layer L (cm)
! LL(L)     Volumetric soil water content in soil layer L at lower limit
!             (cm3/cm3)
! NL        Maximum number of soil layers = 20 
! NLAYR     Actual number of soil layers 
! PORMIN    Minimum pore space required for supplying oxygen to roots for 
!             optimal growth and function (cm3/cm3)
! RLV(L)    Root length density for soil layer L ((cm root / cm3 soil))
! RWU(L)    Root water uptake from soil layer L (cm/d)
! RWUMX     Maximum water uptake per unit root length, constrained by soil 
!             water (cm3[water] / cm [root])
! SAT(L)    Volumetric soil water content in layer L at saturation
!             (cm3 [water] / cm3 [soil])
! SATFAC    Root length weighted soil water excess stress factor ( 0 = no 
!             stress; 1 = saturated stress ) 
! SUMEX     Sum of water excess factor times depth times root length 
!             density 
! SUMRL     Sum of root length density (integrated over depth) 
! SW(L)     Volumetric soil water content in layer L
!             (cm3 [water] / cm3 [soil])
! SWCON1    Constant used in determining root water uptake 
! SWCON2(L) Variable used in determining root water uptake, dependant on 
!             lower limit in layer L 
! SWCON3    Constant used in determining root water uptake 
! SWEXF     Excess water stress factor for layer with deepest roots (0-1) 
! TRWUP     Total potential daily root water uptake (cm/d)
! TSS(L)    Number of days soil layer L has been saturated (d)
!-----------------------------------------------------------------------
!     END SUBROUTINE ROOTWU
!=======================================================================
