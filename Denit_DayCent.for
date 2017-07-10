C=======================================================================
C  Denit_DayCent, Subroutine
C
C  Determines denitrification based on DayCent model

C-----------------------------------------------------------------------
C  Revision history
C  06/12/2014 PG / CHP Written
C-----------------------------------------------------------------------
C  Called : SOIL
C  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
C           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
C=======================================================================

      SUBROUTINE Denit_DayCent (CONTROL, ISWNIT, 
     &    BD, DUL, KG2PPM, newCO2, NLAYR, NO3,        !Input
     &    SNO3, SW,                                   !Input
     &    DLTSNO3,                                    !I/O
     &    CNOX, TNOXD, N2O_data)                      !Output

!-----------------------------------------------------------------------
      USE N2O_mod 
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

      INTEGER DOY, DYNAMIC, L, YEAR, YRDOY
      INTEGER NLAYR

      REAL FLOOD, WFDENIT, XMIN
      REAL SNO3_AVAIL
      REAL DLTSNO3(NL)   
      REAL BD(NL), DUL(NL), KG2PPM(NL) 
      REAL NO3(NL), SNO3(NL), SW(NL)
      
!!!!! daycent variables  PG
      REAL wfps(nl)
      REAL co2_correct(nl), co2PPM(nl)        !wfps_fc(nl), poros(nl), 
      REAL a_coeff, wfps_thres, fDno3, Rn2n2O(NL), fRwfps
      REAL fRno3_CO2, k1, fDCO2, fDwfps, X_inflect
      REAL m, fNo3fCo2
      REAL newCO2(0:nl), dD0_fc(nl)
      real A(4)
!     real RWC
      
      Real ratio1(nl), ratio2(nl)
      INTEGER NDAYS_WET(NL)

      TYPE (N2O_type) N2O_DATA
!          Cumul      Daily       Layer ppm        Layer kg
      REAL CNOX,      TNOXD,      denitrifppm(NL), DENITRIF(NL)  !Denitrification
      REAL CN2,       TN2D,                        n2flux(nl)    !N2
      REAL CN2Odenit, TN2OdenitD, n2odenitppm(NL), n2odenit(nl)  !N2O from denitrification only

      TYPE (ControlType) CONTROL
      DYNAMIC = CONTROL % DYNAMIC
      YRDOY   = CONTROL % YRDOY
      CALL YR_DOY(YRDOY, YEAR, DOY)

!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!     Seasonal cumulative vaules
      CNOX   = 0.0         !denitrification
!     CN2O for denitrification only 
      CN2Odenit   = 0.0    ! N2O added        PG
      CN2    = 0.0         ! N2

      wfps = n2o_data % wfps
 
!     Function for diffusivity
      call DayCent_diffusivity(dD0_fc, DUL, BD, nlayr)

!     Compute the Nitrate effect on Denitrification
!     Changed NO3 effect on denitrification based on paper  "General model for N2O and N2 gas emissions from soils due to denitrification"
!     Del Grosso et. al, GBC   12/00,  -mdh 5/16/00
      A(1) = 9.23
      A(2) = 1.556
      A(3) = 76.91
      A(4) = 0.00222
      
      NDAYS_WET = 0.0

!   temp chp
      write(4000,'(a,/,a)') "DayCent",
     &   "  yrdoy Lyr Wet    wfps  ratio1  ratio2"

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
!     TN2OD = 0.0     ! PG
      TN2OdenitD = 0.0     ! PG
      TN2D  = 0.0
      wfps = n2o_data % wfps
      DENITRIF = 0.0
      N2FLUX   = 0.0
      n2odenit  = 0.0

      DO L = 1, NLAYR
!        wfps(L) = min(1.0, sw(L) / poros(L))
        
!!       following code is Rolston pdf document
!        RWC = SW(L)/SAT(L)                                  
!        if (RWC .GE. 0.8 .AND. RWC. LE .0.9) then
!            WFDENIT = RWC * 2. - 1.6
!        elseif (RWC .GT. 0.9 .AND. RWC .LE. 1.0) then
!            WFDENIT = RWC * 8. -7.
!        else
!            WFDENIT = 0.0
!        endif    
!            
!        WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
!       Daycent denitrification routines PG 17/5/13      

!       CO2ppm is labile C which is derivation of CO2 produced on any day
        if (L == 1) then
          CO2ppm(1) = (newCO2(0)+newCO2(1))*kg2ppm(L)  !first soil layer includes residue layer-chp
        else
          CO2ppm(L) = newCO2(L)*kg2ppm(L)
        endif
      
!       Water Filled Pore Space (WFPS) WFPS_threshold
        If (dD0_fc(L) .GE. 0.15) then 
          WFPS_thres = 0.80
        else
          WFPS_thres = (dD0_fc(L)*250. + 43.)/100.
        endif
        
        if(wfps(L) .LE. WFPS_thres) then
          co2_correct(L) = co2PPM(L)
        else
          if(dD0_fc(L) .GE. 0.15) then
            a_coeff = 0.004
          else 
            a_coeff = -0.1*dD0_fc(L) + 0.019
          endif    
          co2_correct(L)=co2PPM(L)*(1.0+a_coeff*(wfps(L)-WFPS_thres))   !the amount labile C is adjusted taking into account diffusion
        endif  
        
!       Compute the Carbon Dioxide effect on Denitrification fDco2, ppm N
!       Changed CO2 effect on denitrification based on paper "General model for N2O and N2 gas emissions from  soils due to denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 
        fDno3 = (A(2) + (A(3)/PI) * atan(PI*A(4)*(no3(L)-A(1))))  !daycent NO3 factor

        fDco2 = 0.1 * co2_correct(L)**1.27    !daycent labile C factor

        fNO3fCO2 = min (fDco2, fDno3)

!       Compute wfps effect on denitrification, (fDwfps, 0-1)
!       Changed wfps effect on denitrification based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00

        M = min(0.113, dD0_fc(L)) * (-1.25) + 0.145
       
!       The x_inflection calculation should take into account the corrected CO2 concentration, cak - 07/31/02 

        x_inflect = (9.0 - M * co2_correct(L))    ! daycent  X_inflection/adjustment for WFPS response
      
        fDwfps=0.45+(atan(0.6* 3.1415*(10.0*wfps(L)- x_inflect)))/PI   ! daycent WFPS factor
         
        fDwfps = max(0.0, fDwfps)

        denitrifppm(L) = fDwfps * fNO3fCO2   !daycent
C       denitrifppm(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT ! from DSSAT DENIT version, moved here from below PG
        
C       Convert total dentrification, N2O and N2 to kg/ha/d from ppm
        denitrif(L) = denitrifppm(L)/kg2ppm(L)

! *** Moved this section up from below - needs to be done prior to partitioning to N2 and N2O
!       Modify Denitrification rate based on available NO3 & flooding effects 
        DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)
      
!       The minimum amount of NO3 that stays behind in the soil and 
!       cannot denitrify is XMIN.
        XMIN    = 0.       !AJG

!       Check that no more NO3 denitrifies than there is, taking
!       into account what has already been removed by other
!       processes (thus use only negative DLTSNO3 values). This is a
!       protection against negative values at the integration step.
        SNO3_AVAIL = SNO3(L) + AMIN1 (DLTSNO3(L), 0.) - XMIN

!       Take the minimum of the calculated denitrification and the
!       amount of NO3 available for denitrification. 
        DENITRIF(L)  = AMIN1 (DENITRIF(L), SNO3_AVAIL)

!       chp/us 4/21/2006
        IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
!          DENITRIF(L) = SNO3_AVAIL
!         chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!         previously removed 100% NO3/d
          DENITRIF(L) = SNO3_AVAIL * 0.5
        ENDIF

!       chp 4/20/2004   DENITRIF = AMAX1 (DENITRIF, DNFRATE)
        DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)     
! *** End of Moved section
        
! *** Ratio of N2 to N2O        
!       Nitrate effect on the ratio of N2 to N2O. Two approaches, use maximum of ratio1 and ratio2
!       Maximum N2/N2O ratio soil respiration function 
!       Changed the NO3 and CO2 effect on the N2/N2O ratio based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 
!       fRno3_co2 estimates the ratio as a function of electron donor to substrate -mdh 5/17/00

        k1 = max(1.5, 38.4 - 350. * dD0_fc(L))

        fRno3_co2 = max(0.16 * k1, 
     &    k1 * exp(-0.8 * no3(L)/co2_correct(L)))

C       WFPS effect on the N2/N2O Ratio */
C       Changed wfps effect on the N2/N2O ratio based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
C       Del Grosso et. al, GBC   12/00,  -mdh 5/16/00

        fRwfps = max(0.1, 0.015 * wfps(L)*100 - 0.32)
      
C       Compute the N2:N2O Ratio
!       Rn2n2o = max(0.1,fRno3_co2 * fRwfps)
        ratio1(L) = max(0.1,fRno3_co2 * fRwfps)
        
!       Count the number of days that water filled pore space is above 0.80     
        if (wfps(L) >= 0.80) then
            ndays_wet(L) = min(7, ndays_wet(L) + 1)
        else
            ndays_wet(L) = 0
        endif
        
!       modify Rn2n2o based on number of wet days 
        if (ndays_wet(L) > 0) then
            ratio2(L) = -330. + 334 * wfps(L) + 18.4 * ndays_wet(L)
            ratio2(L) = max(ratio2(L),0.0)

!           temp chp
            write(4000,'(i7,2i4,3F8.3)')
     &       yrdoy, L, ndays_wet(L), wfps(L), ratio1(L), ratio2(L)
        else
            ratio2(L) = 0.0
        endif
        
C       Calculate N2O       
!       PG changed n2ofluxppm to n2odenitppm to differentiate n2o from denitrification
!       n2ofluxppm(L) = denitrifppm(L) / (Rn2n2o + 1.0)
        Rn2n2o(L) = max(ratio1(L), ratio2(L)) 
        n2odenit(L) = denitrif(L) / (Rn2n2o(L) + 1.0)

!       PG changed n2oflux to n2odenit to differentiate n2o from denitrification       
        n2flux(L) = denitrif(L) - n2odenit(L)
      
!       Reduce soil NO3 by the amount denitrified and add this to
!       the NOx pool
        DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
        CNOX       = CNOX       + DENITRIF(L)
        TNOXD      = TNOXD      + DENITRIF(L)
        
!       need to differentiate N2O from denitrification        
        CN2Odenit  = CN2Odenit  + n2odenit(L)           ! PG added
        TN2OdenitD = TN2OdenitD + n2odenit(L)         ! PG added

        CN2  = CN2  + N2FLUX(L)            ! PG
        TN2D = TN2D + N2FLUX(L)            ! PG

      END DO   !End of soil layer loop.


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
      N2O_data % TN2OdenitD=TN2OdenitD
      N2O_data % TNOXD    = TNOXD
      N2O_data % DENITRIF = DENITRIF
      N2O_data % n2odenit = n2odenit
      N2O_data % N2FLUX   = N2FLUX

      RETURN
      END SUBROUTINE Denit_DayCent

!=======================================================================


!=======================================================================
!  DayCent_diffusivity, Subroutine
!
!     dD0_fc = f(dul, bd, wfps_fc)
!
!-----------------------------------------------------------------------
!  Revision history
!  06/12/2014 PG / CHP Written
!-----------------------------------------------------------------------
!  Called : SOIL
!  Calls  : Fert_Place, IPSOIL, NCHECK, NFLUX, RPLACE,
!           SOILNI, YR_DOY, FLOOD_CHEM, OXLAYER
!=======================================================================
      subroutine DayCent_diffusivity(dD0_fc, dul, bd, nlayr)      
!      the variable dDO_fc is manually calculated for the moment

      use ModuleDefs
      real, dimension(NL), intent(in) :: dul, bd
      real, dimension(NL), intent(out):: dD0_fc
      integer L, nlayr
      
! From Iurii Shcherbak 6 July 2014
! Linear model
! Dfc = 0.741804 - 0.287059 BD - 0.755057 FC
! Adj R^2 = 0.999412
!  
! Quadratic
! Dfc = 0.906277 - 0.461712 BD + 0.045703 BD^2 - 1.19827 FC +  0.225558 BD FC + 0.292491 FC^2
! Adj R^2 = 0.999997

      do L = 1, Nlayr
!       Linear
!       dD0_fc(L) = 0.741804 - 0.287059 * BD(L) - 0.755057 * FC(L)

!       Quadratic
        dD0_fc(L) = 0.906277 - 0.461712 * BD(L) 
     &                       + 0.045703 * BD(L) * BD(L) 
     &                       - 1.19827  * DUL(L) 
     &                       + 0.225558 * BD(L) * DUL(L) 
     &                       + 0.292491 * DUL(L) * DUL(L)
      enddo

!     Temporary function for diffusivity - set to constant for now
!      dD0_fc = 0.10    ! 0.10 for KT, fixed value since diffusivity routine is not working
!      dD0_fc = 0.16    ! 0.16 for KY, fixed value since diffusivity routine is not working
!      dD0_fc = 0.11    ! 0.11 for Canada (use KT), fixed value since diffusivity routine is not working
!      dD0_fc = 0.21    ! 0.21 for India, fixed value since diffusivity routine is not working
!      dD0_fc = 0.09    ! 0.09 for France (use KT), fixed value since diffusivity routine is not working

      end subroutine DayCent_diffusivity
!=======================================================================


!=======================================================================
! Denit_DayCent Variables 
!-----------------------------------------------------------------------

!***********************************************************************
