C=======================================================================
C  VertDiffusion, Subroutine, C.H.Porter
C  Calculates diffusion of soil water - upward and downward flow. 
C-----------------------------------------------------------------------
C  REVISION       HISTORY
!  2023-02-16 CHP extracted the vertical diffusion code from the 2D 
!                 Drainage.for module.
C=======================================================================

      SUBROUTINE VertDiffusion(SOILPROP, SW_AVAIL, SWDELTU)  
!                                  !Inputs         !Output

!-----------------------------------------------------------------------
      USE ModuleDefs
      IMPLICIT NONE
      EXTERNAL K_unsat, Diffus_Coef
      SAVE

!-----------------------------------------------------------------------
!     Interface variables:
!-----------------------------------------------------------------------
!     Input:
!     INTEGER, INTENT(IN) :: DYNAMIC
      TYPE (SoilType), INTENT(IN) :: SOILPROP
      REAL, DIMENSION(NL), INTENT(IN) :: SW_AVAIL
      REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTU

!-----------------------------------------------------------------------
      CHARACTER*6, PARAMETER :: ERRKEY = 'Diffus' 

      INTEGER L, NLAYR, NT, LIMIT_Diff, LimitingL
      REAL DiffusV, K_unsat, Diffus_Coef
      REAL DeltaT, TimeIncr, StartTime, EndTime !, T
      REAL DeltaSe, DeltaSW, SW_check, Excess, Deficit
      REAL, DIMENSION(NL) :: DLAYR, DUL, DS, Ksat, SAT, Se, WCr
      REAL, DIMENSION(NL) :: Diffus, Kunsat, Thick, V_diff, SW_temp

!     vanGenuchten parameters
      REAL, DIMENSION(NL) :: alphaVG, mVG, nVG

!***********************************************************************
!***********************************************************************
!     SEASONAL INITIALIZATION
!***********************************************************************
!      IF (DYNAMIC .EQ. SEASINIT) THEN
!-----------------------------------------------------------------------
!     Transfer values from constructed data types into local variables.
      DLAYR   = SOILPROP % DLAYR
      DS      = SOILPROP % DS
      DUL     = SOILPROP % DUL 
      Ksat    = SOILPROP % SWCN 
      NLAYR   = SOILPROP % NLAYR
      SAT     = SOILPROP % SAT
      WCR     = SOILPROP % WCR
      alphaVG = SOILPROP % alphaVG
      mVG     = SOILPROP % mVG
      nVG     = SOILPROP % nVG

      Diffus = 0.0

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS
!***********************************************************************
!      ELSEIF (DYNAMIC .EQ. RATE) THEN
!-----------------------------------------------------------------------
      SW_temp = SW_AVAIL

!     Limits diffusion to the unsaturated zone. 
      LIMIT_Diff = NLAYR
      DO L = NLAYR, 1, -1
        IF ((SW_AVAIL(L) - DUL(L)) > (0.9 * (SAT(L) - DUL(L)))) THEN
          LIMIT_Diff = L - 1
        ELSE 
          EXIT
        ENDIF
      ENDDO

      DO L = 1, NLAYR
!       Layer thickness
        IF (L == 1) THEN
          Thick(L) = DS(L)
        ELSE
          Thick(L) = DS(L) - DS(L-1)
        ENDIF
      ENDDO
        
      StartTime = 0.0 !hours
      EndTime = 0.0 !hours
      NT = 0
!-------------------------------------------------
!     Loop through time steps today
      DO WHILE (StartTime < 23.95) !Close to 24 hours
        NT = NT + 1
        StartTime = EndTime
        V_diff = 0.0
        TimeIncr = 24. !hours
!-------------------------------------------------
!       Calculate parameters for diffusion calculations
!       Need to do this in a separate loop because the next loop
!         uses values for layers above and below.
        DO L = 1, NLayr
!         IF (L > LIMIT_Diff) EXIT
        
!         Normalized soil water content
          Se(L) = (SW_temp(L) - WCr(L))/(SAT(L) - WCr(L))
          Se(L) = MIN(1.0, Se(L))
          Se(L) = MAX(0.0, Se(L))
        
!         Unsaturated hydraulic conductivity
          Kunsat(L)= K_unsat(Ksat(L), mVG(L), Se(L))
        
!         Diffusion coefficient 
          Diffus(L)= Diffus_Coef(Ksat(L), alphaVG(L), mVG(L), SAT(L),
     &      Se(L), WCr(L))

          IF (L < LIMIT_Diff .AND. 
     &        Diffus(L) > 1.E-9 .AND. 
     &        Kunsat(L) > 1.E-9) THEN
            DeltaT = 1./(
!    &        2.*Diffus(L)/(Width(L)*Width(L)) + 
     &        2.*Diffus(L)/(Thick(L)*Thick(L)) + 
     &        Kunsat(L)/Thick(L))  !hr 

            IF (DeltaT < TimeIncr) THEN            
              TimeIncr = DeltaT
              LimitingL = L
            ENDIF
          ENDIF
        ENDDO

        EndTime = StartTime + TimeIncr
        IF (EndTime > 24.) THEN
          EndTime = 24.
        ENDIF
!        write(*,*) NT, StartTime, EndTime

!-------------------------------------------------
!       Vertical diffusion flow for this time step
        DO L = 1, NLAYR
!         -------------------------------------------------------------
          DiffusV = SQRT(Diffus(L) * Diffus(L+1))
        
!         The normalized water content is the driving force for non-homogeneous soils
          DeltaSe = (Se(L+1) - Se(L)) * 
     &               0.5 * (SAT(L+1) - WCr(L+1) + SAT(L) - WCr(L))
        
!         Flux of water from layer L+1 to layer L in this time step
          V_diff(L) =  DiffusV / ((Thick(L) + Thick(L+1)) * 0.5)
!           cm   =  cm2/hr  /         cm               
     &              * DeltaSe  *  TimeIncr 
!                   * mm3/mm3  *     hr         

!         To account for large time step, arbitrarily reduce V_diff, need to play around with the reduction factor
          V_diff(L) = V_diff(L) / 10.0

          IF (L == 1) THEN
            DeltaSW = V_diff(L) / Thick(L)
          ELSE ! L > 1
!           Change in SW for this layer = what comes up from layer below minus what goes up to layer above
            DeltaSW = (V_diff(L)-V_diff(L-1)) / Thick(L)
          ENDIF

          SW_check = SW_temp(L) + DeltaSW

!         Check for too much movement of water in one time step
          IF (DeltaSW > 0.0) THEN
!           DeltaSW is positive, net flow is upward from the layer below.
            Excess = SW_check - SAT(L)
            IF (Excess > 0.0) THEN
!             Too much water moved into layer, get rid of excess
              DeltaSW = DeltaSW - Excess
              IF (L == 1) THEN
                V_diff(L) = DeltaSW * Thick(L)
              ELSE
                V_diff(L) = DeltaSW * Thick(L) + V_Diff(L-1)
              ENDIF
            ENDIF
            
          ELSEIF (DeltaSW < 0.0) THEN
!           DeltaSW is negative, net flow is downward to the layer below.
            Deficit = WCr(L) - SW_check
            IF (Deficit > 0.0) THEN
!             Too much water was moved out of layer, need to reduce the negative flow
!             V_diff(L) is negative in this case, ADD the deficit amount
              DeltaSW = DeltaSW + Deficit
              IF (L == 1) THEN
                V_diff(L) = DeltaSW * Thick(L)
              ELSE
                V_diff(L) = DeltaSW * Thick(L) + V_Diff(L-1)
              ENDIF
            ENDIF
          ENDIF

          SW_temp(L) = SW_temp(L) + DeltaSW
!         SW after diffusion limited to between residual and saturation
          SW_temp(L) = MIN(SW_temp(L), SAT(L))
          SW_temp(L) = MAX(SW_temp(L), WCR(L))
        ENDDO 
      ENDDO   !Time loop

      DO L = 1, NLAYR
        SWDELTU(L) = SW_temp(L) - SW_AVAIL(L)
      ENDDO 

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
!      ENDIF
!-----------------------------------------------------------------------

      RETURN
      END SUBROUTINE VertDiffusion
!=====================================================================

C=====================================================================
!     VertDiffusion VARIABLE DEFINITIONS: (updated Oct 2009)
!-----------------------------------------------------------------------
! DiffusH    Horizontal diffusivity in cm2/hr
! DiffusV    Vertical diffusivity in cm2/hr
! Diff_limit Maximum water which can be removed from cell by diffusion cm2
! DS(i)      Depth of buttom of ith layer
! Grav_limit Maximum water which can be removed from cell by gravity flow cm2
! H_in(Row,Col)    2D Horizontal water amount into cell in current time step in cm2
! H_out(Row,Col)   2D Horizontal water amount out of cell in current time step in cm2
! LatFlow_ts    Total inward lat flow for all cells for current time step. It is in mm finally
! K_unsat(Row,Col) Un-saturated hydraulic conductivity in cm/hr
! SWFh_ts(Row,Col) 2D Horizontal water amount out of cell in current time step in cm2
! SWFv_ts(Row,Col) 2D Vertical water amount out of cell in current time step in cm2
! SWV_D(Row,Col)   Double precision soil water content at the beginning of time step in cm2
! SWV_ts(Row,Col)  Soil water content at the end of time step in cm2
! TargetSWV  Balanced soil water content at given hight
! ThetaCap An array of volumetric soil water contents at the midpoint of each soil layer.
!          Calculated from the water characteristic curve at the height above the
!          water table. At equilibrium state, soil matric potential h = water table depth z
! Thick(Row,Col) Cell thickness in cm[soil]
! TimeIncr       Dynamic time step within a day in min.
! V_diff  in cm2 Vertical water amount into cell due to diffusivity in current time step in cm2
! V_grav in cm2  Vertical water amount into cell due to gravity in current time step in cm2
! V_in(Row,Col)  2D Vertical water amount into cell in current time step in cm2
! V_out(Row,Col) 2D Vertical water amount out of cell in current time step in cm2
!-----------------------------------------------------------------------
!     END SUBROUTINE Drainage_2D
!=======================================================================

!=====================================================================
      Function Diffus_Coef(Ksat, alphaVG, mVG, SAT, Se, WCr)
!     Computes diffusivity coefficient
!       based on water content, water holding capacity
!     Parameters for diffusion coefficient from RETC code
!     Diffusivity coefficient in cm2/hr

!     ----------------------------------------------------------------
      Implicit None
      REAL Diffus_Coef, SAT, Ksat, WCr

!     RETC
      REAL Coef1, Exponent, Coef2, Coef3, Se
      REAL alphaVG, mVG

      REAL, PARAMETER :: DiffusCap = 417. !cm2/hr  !Hillel 1. m2/d 
      REAL, PARAMETER :: L = 0.5

!--------------------------------------------------------------
      IF (Se > 0.99) THEN
        Diffus_Coef = DiffusCap
      ELSEIF (Se > 1.E-9) THEN
        Coef1 = (1-mVG)*Ksat / (alphaVG*mVG*(SAT-WCr))
        Coef2 = (1-Se**(1./mVG))
        Coef3 = Coef2**(-mVG) + Coef2**mVG - 2.
        Exponent = L - 1./mVG
        Diffus_Coef = (Coef1 * Se ** Exponent * Coef3)  !cm2/hr
      ELSE
        Diffus_Coef = 0.0
      ENDIF

!--------------------------------------------------------------
!     Upper limit on diffusion = DiffusCap
      IF (Diffus_Coef > DiffusCap) THEN
        Diffus_Coef = DiffusCap
      ENDIF

!     Lower limit = 0.
      IF (Diffus_Coef < 1.E-10) THEN
        Diffus_Coef = 0.0
      ENDIF

      RETURN
      END Function Diffus_Coef
!=====================================================================
!=======================================================================
!  K_unsat, function, 
!  Method 1.Based on paper "Soil Water Characteristic Estimates by Texture and Organic 
!  Matter for Hydrologic Solutions" by K. E. Saxton and W. J. Rawls, Aug. 2006
!  If soil structure is giving, use soil structure to calculate Ksat, otherwise
!  using LL, DUL and SAT to calculate Ksat
!  Method 2: RETC program: calculate hydraulic conductivity uses Mualem's model
!  m = 1- 1/n
!-----------------------------------------------------------------------
!  REVISION HISTORY
!  
!-----------------------------------------------------------------------
!  Called by: Subroutine RETC_VG
!  Calls:     None
!=====================================================================
      Function K_unsat(Ksat, mVG, Se)
!     Computes unsaturated hydraulic conductivity 
!       based on Ksat, water content, water holding capacity
!       in cm/hr
!     ----------------------------------------------------------------
      Implicit None
      REAL K_unsat, Ksat
      REAL Se, mVG
!     L is a pore-connectivity parameter, be about 0.5 as an average for many soil
      REAL, PARAMETER :: Lp = 0.5

      IF (Se >= .9999) THEN
        K_unsat = Ksat
      ELSE
!        RETC program: calculate hydraulic conductivity uses Mualem's model
        ! Eq. 31 in RETC.pdf
        K_unsat = Ksat * (Se**Lp) * (1. -(1. - Se **(1./mVG) )**mVG)**2.
        ! in cm/h
        K_unsat = Max(0., K_unsat) 
        K_unsat = Min(Ksat, K_unsat) 
        IF (K_unsat < 1.E-10) THEN
          K_unsat = 0.0
        ENDIF
      ENDIF

      RETURN
      END Function K_unsat
!=====================================================================
!-----------------------------------------------------------------------
!     Kunsat VARIABLE DEFINITIONS:
!-----------------------------------------------------------------------
! WCr      Residual water content
!-----------------------------------------------------------------------
!     END FUNCTION Kunsat
!=======================================================================
