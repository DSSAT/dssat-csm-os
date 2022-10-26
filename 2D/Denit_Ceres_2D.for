C=======================================================================
C  Denit_Ceres_2D, Subroutine
C
C  Determines denitrification based on Ceres model

C-----------------------------------------------------------------------
C  Revision history
C  06/12/2014 PG / CHP Written
C  07/09/2018 MZ Modified for 2D
C-----------------------------------------------------------------------
C  Called : SOIL
C  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
C           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
C=======================================================================

      SUBROUTINE Denit_Ceres_2D (CONTROL, ISWNIT,  
!    &    DUL, FLOOD, KG2PPM, LITC, NLAYR, NO3_2D,    !Input
     &    DUL, KG2PPM, LITC, NLAYR, NO3_2D,           !Input
     &    SAT, SSOMC, SNO3_2D, ST, SWV, Cells,        !Input
     &    ColFrac,                                    !Input
     &    DLTSNO3_2D,                                 !I/O
!    &    CNOX, TNOXD, DENITRIF, N2O_data)            !Output
     &    CNOX, TNOXD, DENITRIF)                      !Output

!-----------------------------------------------------------------------
      USE Cells_2D
!     USE N2O_mod 
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

      INTEGER DYNAMIC, L, NLAYR, J
      Type (CellType) Cells(MaxRows,MaxCols)
      real, dimension(MaxRows,MaxCols) :: ColFrac

      REAL CW, XMIN !,FLOOD
      REAL TFDENIT, WFDENIT
      REAL ST(NL), SNO3_AVAIL
      REAL, DIMENSION(0:NL) :: LITC, SSOMC

      REAL DUL(NL), KG2PPM(NL) 
      REAL SAT(NL)
      REAL, DIMENSION(MaxRows,MaxCols) :: NO3_2D, SNO3_2D, DLTSNO3_2D, 
     &    SWV
      INTEGER, DIMENSION(MaxRows,MaxCols) :: DLAG_2D
      REAL wfps(NL), Rn2n2o(MaxRows,MaxCols)

      Real ratio1(MaxRows,MaxCols), ratio2(NL), Rn2odenit
      INTEGER NDAYS_WET(NL), yrdoy

!      TYPE (N2O_type)    N2O_DATA
!          Cumul      Daily     Layer kg
      REAL CNOX,      TNOXD,    DENITRIF(MaxRows,MaxCols)   !Denitrification
!      REAL CN2,       TN2D,     N2FLUX(MaxRows,MaxCols)     !N2
!      REAL CN2Odenit, TN2OdenitD, n2odenit(MaxRows,MaxCols) !N2O 

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

!      wfps = n2o_data % wfps
      
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Seasonal cumulative vaules
      CNOX   = 0.0    !denitrification
!      CN2Odenit   = 0.0    ! N2O added        PG
!      CN2    = 0.0

      NDAYS_WET = 0.0

!!   temp chp
!      write(4000,'(a,/,a)') "Ceres",
!     &   "  yrdoy Lyr Wet    wfps  ratio1  ratio2"

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------
      TNOXD = 0.0
!     TN2OdenitD = 0.0     ! PG
!     TN2D  = 0.0
      DENITRIF = 0.0
!     N2FLUX   = 0.0
!     n2odenit  = 0.0

      DO J = 1, NColsTot
        DO L = 1, NRowsTot

!         Denitrification only occurs if there is nitrate, SW > DUL and
!         soil temperature > 5.
          IF (NO3_2D(L, J) .GT. 0.01 .AND. SWV(L, J) .GT. DUL(L) .AND.
     &       ST(L) .GE. 5.0) THEN

          CW = 24.5 + 0.0031 * (SSOMC(L) + 0.2 * LITC(L)) * KG2PPM(L)

!         Temperature factor for denitrification.
          TFDENIT = 0.1 * EXP (0.046 * ST(L))
          TFDENIT = AMAX1 (AMIN1 (TFDENIT, 1.), 0.)

!         Water factor for denitrification: only if SW > DUL.
          WFDENIT = 1. - (SAT(L) - SWV(L, J)) / (SAT(L) - DUL(L))
          WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)

          IF (WFDENIT .GT. 0.0) THEN
            DLAG_2D(L, J) = DLAG_2D(L, J) + 1
          ELSE
            DLAG_2D(L, J) = 0
          ENDIF

          IF (DLAG_2D(L, J) .LT. 5) THEN
            WFDENIT = 0.0
          ENDIF

!         Denitrification rate
          DENITRIF(L, J) = 6.0 * 1.E-04 * CW * NO3_2D(L, J) *WFDENIT *
     &                 TFDENIT / KG2PPM(L)       
          DENITRIF(L, J) = AMAX1 (DENITRIF(L, J), 0.0)

!         The minimum amount of NO3 that stays behind in the soil and 
!         cannot denitrify is XMIN.
          XMIN    = 0.       !AJG

!         Check that no more NO3 denitrifies than there is, taking
!         into account what has already been removed by other
!         processes (thus use only negative DLTSNO3 values). This is a
!         protection against negative values at the integration step.
          SNO3_AVAIL = SNO3_2D(L, J) + AMIN1 (DLTSNO3_2D(L, J), 0.) - 
     &                    XMIN

!         Take the minimum of the calculated denitrification and the
!         amount of NO3 available for denitrification. 
          DENITRIF(L, J)  = AMIN1 (DENITRIF(L, J), SNO3_AVAIL)
          IF (DENITRIF(L, J) > SNO3_AVAIL) THEN
            DENITRIF(L, J) = SNO3_AVAIL
          ENDIF

!!         chp/us 4/21/2006
!          IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
!!            DENITRIF(L, J) = SNO3_AVAIL
!!           chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!!           previously removed 100% NO3/d
!            DENITRIF(L, J) = SNO3_AVAIL * 0.5
!          ENDIF
          DENITRIF(L, J) = AMAX1 (DENITRIF(L, J), 0.0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!C         Compute the N2:N2O Ratio
!! Calculation of n2odenit based on ratio (N2O/total denit) determined from original DayCent dataset of DelGrosso (PG)
!! assuming denitrif = N2O + N2O
!!         n2odenit(L, J) = NO3_2D(L, J)/(NO3(L, J)+30.)*DENITRIF(L, J)   ! PG
!          Rn2odenit = NO3_2D(L, J)/(NO3_2D(L, J)+30.)
!          ratio1(L, J) = 1./Rn2odenit - 1.
!         
!!         Count the number of days that water filled pore space is above 0.80     
!!         MZ : need to upgrade wfps to 2D in the future?
!          if (wfps(L) >= 0.80) then
!            ndays_wet(L) = min(7, ndays_wet(L) + 1)
!          else
!            ndays_wet(L) = 0
!          endif
!        
!!         modify Rn2n2o based on number of wet days 
!          if (ndays_wet(L) > 0) then
!            ratio2(L) = -330. + 334 * wfps(L) + 18.4 * ndays_wet(L)
!            ratio2(L) = max(ratio2(L),0.0)
!
!          else
!            ratio2(L) = 0.0
!          endif
!
!          Rn2n2o(L, J) = max(ratio1(L, J), ratio2(L)) 
!          n2odenit(L, J) = DENITRIF(L, J) / (Rn2n2o(L, J) + 1.0)
!          N2FLUX(L, J) = DENITRIF(L, J) - n2odenit(L, J)   ! PG

!         Reduce soil NO3 by the amount denitrified and add this to
!         the NOx pool
          DLTSNO3_2D(L, J) = DLTSNO3_2D(L, J) - DENITRIF(L, J)
          ! JZW? what is teh difference betwee TNOX and TNOXD
          CNOX  = CNOX  + DENITRIF(L, J) * ColFrac(L, J) 
          TNOXD = TNOXD + DENITRIF(L, J) * ColFrac(L, J) 
!!         need to differentiate N2O from denitrification        
!          CN2Odenit  = CN2Odenit  + n2odenit(L, J) * ColFrac(L, J)  ! PG added
!          TN2OdenitD = TN2OdenitD + n2odenit(L, J) * ColFrac(L, J)  ! PG added
!          CN2  = CN2  + N2FLUX(L, J) * ColFrac(L, J)                ! PG
!          TN2D = TN2D + N2FLUX(L, J) * ColFrac(L, J)                ! PG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ELSE
!           IF SW, ST OR NO3 FALL BELOW CRITICAL IN ANY LAYER RESET LAG EFFECT.
            DENITRIF(L, J) = 0
            DLAG_2D(L, J) = 0      !REVISED-US
          ENDIF   !End of IF block on denitrification.

        END DO   !End of soil column loop.
      End Do !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
!      N2O_data % CN2      = CN2
!      N2O_data % CN2Odenit= CN2Odenit
!      N2O_data % CNOX     = CNOX
!      N2O_data % TN2D     = TN2D
!      N2O_data % TN2OdenitD= TN2OdenitD
!      N2O_data % TNOXD    = TNOXD
!      CAll Interpolate2Layers_2D(DENITRIF, Cells%Struc, NLAYR,  !input
!     &         N2O_data % DENITRIF)                             !Output
!      CAll Interpolate2Layers_2D(n2odenit, Cells%Struc, NLAYR,  !input
!     &         N2O_data % n2odenit)                             !Output
!      CAll Interpolate2Layers_2D(N2FLUX, Cells%Struc, NLAYR,    !input
!     &         N2O_data % N2FLUX)                               !Output

      RETURN
      END SUBROUTINE Denit_Ceres_2D

!=======================================================================
! Denit_Ceres Variables 
!-----------------------------------------------------------------------
      
! DENITRIF(L)   Denitrification rate in soil layer L (kg [N] / ha / d)
! NO3(L)        Nitrate in soil layer L (µg[N] / g[soil])

!***********************************************************************

