C=======================================================================
C  Denit_Ceres, Subroutine
C
C  Determines denitrification based on Ceres model

C-----------------------------------------------------------------------
C  Revision history
C  06/12/2014 PG / CHP Written
C-----------------------------------------------------------------------
C  Called : SOIL
C  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
C           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
C=======================================================================

      SUBROUTINE Denit_Ceres (CONTROL, ISWNIT,  
     &    DUL, KG2PPM, LITC, NLAYR, NO3, SAT,         !Input
     &    SSOMC, SNO3, ST, SW,                        !Input
     &    DLTSNO3,                                    !I/O
     &    CNOX, TNOXD, N2O_data)                      !Output

!-----------------------------------------------------------------------
      USE GHG_mod 
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

      INTEGER DYNAMIC, L, NLAYR

      REAL CW, XMIN
      REAL TFDENIT, WFDENIT
      REAL ST(NL), SNO3_AVAIL
      REAL, DIMENSION(0:NL) :: LITC, SSOMC

      REAL DLAG(NL), DLTSNO3(NL), DUL(NL), KG2PPM(NL) 
      REAL NO3(NL), SAT(NL), SNO3(NL), SW(NL)
      REAL wfps(NL), Rn2n2O(NL)

      Real ratio1(nl), ratio2(NL), Rn2odenit
      INTEGER NDAYS_WET(NL), yrdoy

      TYPE (N2O_type)    N2O_DATA
!          Cumul      Daily     Layer kg
      REAL CNOX,      TNOXD,    DENITRIF(NL)  !Denitrification
      REAL CN2,       TN2D,     n2flux(nl)    !N2
      REAL CN2Odenit, TN2OdenitD, n2odenit(nl)   !N2O 

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY

      wfps = n2o_data % wfps
      
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Seasonal cumulative vaules
      CNOX   = 0.0    !denitrification
      CN2Odenit   = 0.0    ! N2O added        PG
      CN2    = 0.0

      NDAYS_WET = 0.0

!     CHP added 2023-03-21
      DLAG = 0.0

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
      TN2OdenitD = 0.0     ! PG
      TN2D  = 0.0
      DENITRIF = 0.0
      N2FLUX   = 0.0
      n2odenit  = 0.0

      DO L = 1, NLAYR

!       Denitrification only occurs if there is nitrate, SW > DUL and
!       soil temperature > 5.
        IF (NO3(L) .GT. 0.01 .AND. SW(L) .GT. DUL(L) .AND.
     &       ST(L) .GE. 5.0) THEN

!         Water extractable soil carbon: estimated according to
!         Rolston et al. 1980, as cited in Godwin & Jones 1991 (ASA
!         monograph #31). Calculate carbohydrate carbon as 40% of the
!         carbohydrate pool.
C-UPS     Corrected per e-mail 03/29/00
!         CW = 24.5 + 0.0031 * (SSOMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L)

!     ----------------------------------------------------------------
!11/18/2003 UPS: THE NEW NTRANS SHOULD READ: 
!         CW = 24.5 + {0.0031 * SSOMC(L) + 0.4 * FPOOL(L,1)} * KG2PPM(L) 
!            = 24.5 + AVAILABLE CARBON FROM HUMIC FRACTION + FRESH C from  CARBOHYDRATE POOL 

!NOTES: 1. ONLY THE HUMIC C IS MULTIPLIED BY 0.0031 
!       2. SOILC IN GODWIN&JONES INCLUDED BOTH HUMIC C AND FRESH (LITTER POOL C) 
!       3. WE ARE USING ONLY THE CARBOHYDRATE POOL (*0.4 TO C) = ALL AVAILABLE 
!       4. FPOOL is still kg of Organic matter/ha (and not kg C/ha)?? 

!     SO WE NEED TO FIX BOTH DSSAT4 AND GODWIN AND SINGH EQN TO THE ABOVE. 

!          CW = 24.5 + (0.0031 * SSOMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L) 

!     The above removed on 1/14/2004 as per email from AJG and UPS

!     ----------------------------------------------------------------
!     DENITRIFICATION - CORRECTIONS - 13 Jan 2004 (US)
!         From NTRANS:
!          CW = 24.5 + 0.0031 * (HUMC(L) + 0.4 * FPOOL(L,1)) * KG2PPM(L)
!     ----------------------------------------------------------------

!        From Century:
!        CHP changed 1/14/2004 per email from UPS / AJG
          CW = 24.5 + 0.0031 * (SSOMC(L) + 0.2 * LITC(L)) * KG2PPM(L)
!     ----------------------------------------------------------------
!
!     The DENITRIF or DNRATE calculations are identical in NTRANS 
!             (DSSAT4) and Godwin and Singh:
!
!     DENITRIF = {6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG }/ KG2PPM(L)
!     -- AS in NTRANS (DSSAT4)
!
!         = {in concentration unit}/KG2PPM
!         = kg N (denitrified)
!         = {in conc unit }/(10/BD*DLAYR)
!
!     DENITRIF = 6.0*1.E-05 * CW * NO3(L) * WFDENIT * TFDENIT * DLAG*BD(L)*DLAYR(L)
!     -- AS in GODWIN & SINGH
!
!     NOTE: CW is in concentration unit!!  Extractable C concentration.
!
!     CW = (SOILC*KG2PPM(L))*0.0031 + 24.5
!
!       where SOILC = SSOMC(L) + 0.4 * FOM(L) in kg/ha - Origianl definition
!        Later corrected to:
!        SOILC = SSOMC(L) + 0.4 * FPOOL(L,1)  -- because only carbohydrate 
!             pool from the FOM is assumed to provide labile/extractable C.
! 
!     CW   = ({SSOMC(L) + 0.4 * FPOOL(L,1)} * KG2PPM(L))*0.0031 + 24.5
!
!     The equations in Godwin and Jones as well as Godwin and Singh are incorrect!!

!     ----------------------------------------------------------------
!         Temperature factor for denitrification.
          TFDENIT = 0.1 * EXP (0.046 * ST(L))
          TFDENIT = AMAX1 (AMIN1 (TFDENIT, 1.), 0.)

!         Water factor for denitrification: only if SW > DUL.
          WFDENIT = 1. - (SAT(L) - SW(L)) / (SAT(L) - DUL(L))
          WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)

          IF (WFDENIT .GT. 0.0) THEN
            DLAG(L) = DLAG(L) + 1
          ELSE
            DLAG(L) = 0
          ENDIF

          IF (DLAG(L) .LT. 5) THEN
            WFDENIT = 0.0
          ENDIF

!         Denitrification rate
C-UPS     Corrected per e-mail 03/29/00
!         DLAG REMOVED REVISED-US 4/20/2004
!         DENITRIF in kg/ha
          DENITRIF(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * 
     &                 TFDENIT / KG2PPM(L)       
          DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)

!         The minimum amount of NO3 that stays behind in the soil and 
!         cannot denitrify is XMIN.
!         XMIN    = 0.25 / KG2PPM(L)
          XMIN    = 0.       !AJG

!         Check that no more NO3 denitrifies than there is, taking
!         into account what has already been removed by other
!         processes (thus use only negative DLTSNO3 values). This is a
!         protection against negative values at the integration step.
          SNO3_AVAIL = SNO3(L) + AMIN1 (DLTSNO3(L), 0.) - XMIN

!         Take the minimum of the calculated denitrification and the
!         amount of NO3 available for denitrification. 
!         DENITRIF(L)  = AMIN1 (DENITRIF(L), SNO3_AVAIL)
          IF (DENITRIF(L) > SNO3_AVAIL) THEN
            DENITRIF(L) = SNO3_AVAIL
          ENDIF

C         If flooded, lose all nitrate --------REVISED-US
!          IF (FLOOD .GT. 0.0) THEN
!            !DNFRATE = SNO3(L) - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!            DNFRATE = SNO3_AVAIL - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!          ELSE
!            DNFRATE = 0.0
!          ENDIF

!     2023-07-28 Mathilde Dionisi, Myriam Adam - flooded denitrification at same rates as upland
!!         chp/us 4/21/2006
!          IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
!!            DENITRIF(L) = SNO3_AVAIL
!!           chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!!           previously removed 100% NO3/d
!            DENITRIF(L) = SNO3_AVAIL * 0.5
!          ENDIF

!chp 4/20/2004   DENITRIF = AMAX1 (DENITRIF, DNFRATE)
          DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C         Compute the N2:N2O Ratio
! Calculation of n2odenit based on ratio (N2O/total denit) determined from original DayCent dataset of DelGrosso (PG)
! assuming denitrif = N2O + N2
!         n2odenit(L) = NO3(L)/(NO3(L)+30.)*DENITRIF(L)   ! PG
          Rn2odenit = NO3(L)/(NO3(L)+30.)
          ratio1(L) = 1./Rn2odenit - 1.
         
!         Count the number of days that water filled pore space is above 0.80     
          if (wfps(L) >= 0.80) then
            ndays_wet(L) = min(7, ndays_wet(L) + 1)
          else
            ndays_wet(L) = 0
          endif
        
!         modify Rn2n2o based on number of wet days 
          if (ndays_wet(L) > 0) then
            ratio2(L) = -330. + 334 * wfps(L) + 18.4 * ndays_wet(L)
            ratio2(L) = max(ratio2(L),0.0)
          else
            ratio2(L) = 0.0
          endif

          Rn2n2o(L) = max(ratio1(L), ratio2(L)) 
          n2odenit(L) = denitrif(L) / (Rn2n2o(L) + 1.0)
          N2FLUX(L) = DENITRIF(L) - n2odenit(L)   ! PG

!         Reduce soil NO3 by the amount denitrified and add this to
!         the NOx pool
          DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
          CNOX       = CNOX       + DENITRIF(L)
          TNOXD      = TNOXD      + DENITRIF(L)
!         need to differentiate N2O from denitrification        
          CN2Odenit  = CN2Odenit  + n2odenit(L)         ! PG added
          TN2OdenitD = TN2OdenitD + n2odenit(L)         ! PG added
          CN2  = CN2  + N2FLUX(L)            ! PG
          TN2D = TN2D + N2FLUX(L)            ! PG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ELSE
!         IF SW, ST OR NO3 FALL BELOW CRITICAL IN ANY LAYER RESET LAG EFFECT.
          DLAG(L) = 0      !REVISED-US
        ENDIF   !End of IF block on denitrification.

      ENDDO  !Soil layer loop

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      N2O_data % CN2      = CN2
      N2O_data % CN2Odenit= CN2Odenit
      N2O_data % CNOX     = CNOX
      N2O_data % TN2D     = TN2D
      N2O_data % TN2OdenitD= TN2OdenitD
      N2O_data % TNOXD    = TNOXD
      N2O_data % DENITRIF = DENITRIF
      N2O_data % n2odenit = n2odenit
      N2O_data % N2FLUX   = N2FLUX

      RETURN
      END SUBROUTINE Denit_Ceres

!=======================================================================
! Denit_Ceres Variables 
!-----------------------------------------------------------------------
      
! DENITRIF(L)   Denitrification rate in soil layer L (kg [N] / ha / d)
! NO3(L)        Nitrate in soil layer L (�g[N] / g[soil])

!***********************************************************************

