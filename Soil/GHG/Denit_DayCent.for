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
     &    dD0, newCO2, NO3, SNO3, SOILPROP,       !Input
     &    DLTSNO3,                                !I/O
     &    CNOX, TNOXD, N2O_data)                  !Output
!-----------------------------------------------------------------------
      USE GHG_mod 
      USE ModuleData
      IMPLICIT  NONE
      EXTERNAL YR_DOY
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

      INTEGER DOY, DYNAMIC, L, YEAR, YRDOY
      INTEGER NLAYR

      REAL FLOOD, XMIN    !, WFDENIT
      REAL SNO3_AVAIL
      REAL DLTSNO3(NL)   
      REAL BD(NL), DUL(NL), KG2PPM(NL)
      REAL NO3(NL), SNO3(NL), DLAYR(NL), DS(NL)   !, SW(NL)
      
!!!!! daycent variables  PG
      REAL wfps(nl)
      REAL co2_correct(nl), co2PPM(nl)        !wfps_fc(nl), poros(nl), 
      REAL a_coeff, wfps_thres, fDno3, Rn2n2O(NL), fRwfps
      REAL fRno3_CO2, k1, fDCO2, fDwfps, X_inflect
      REAL m, fNo3fCo2
      REAL newCO2(0:nl), dD0(nl)
      real A(4)
!     real RWC
      real min_nitrate
      
      Real ratio1(nl), ratio2(nl)
      INTEGER NDAYS_WET(NL)
      TYPE (SOILTYPE) SOILPROP

      Real Denit_depth, cumdep, frac_layer
      Integer DD_layer

      TYPE (N2O_type) N2O_DATA
!          Cumul      Daily       Layer ppm        Layer kg
      REAL CNOX,      TNOXD,      denitrifppm(NL), DENITRIF(NL)  !Denit
      REAL CN2,       TN2D,                        n2flux(nl)    !N2
!     N2O from denitrification only
      REAL CN2Odenit, TN2OdenitD,                  n2odenit(nl)  

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
 
!     Compute the Nitrate effect on Denitrification
!     Changed NO3 effect on denitrification based on paper  
!     "General model for N2O and N2 gas emissions from soils 
!     due to denitrification"
!     Del Grosso et. al, GBC   12/00,  -mdh 5/16/00
      A(1) = 9.23
      A(2) = 1.556
      A(3) = 76.91
      A(4) = 0.00222
      
      NDAYS_WET = 0.0
      min_nitrate = 0.1

!     Calculate the number of top layers to accelerate s
!     slow denitrification as in DayCent denitrify.c
      NLAYR = SOILPROP % NLAYR
      DLAYR = SOILPROP % DLAYR
      DS    = SOILPROP % DS

      Denit_depth = 30.   !cm depth for accelerated dentitrification
      DD_layer = 1        !layer number at depth = Denit_depth
!     Save soil layer at Denit_depth 
!     (if it's at least half the layer thickness)
      cumdep = 0.0
      do L = 1, nlayr
        cumdep = cumdep + dlayr(L)
        if (cumdep < Denit_depth) then
          cycle
        elseif (L > 1) then
          frac_layer = (Denit_depth - ds(L-1))/(dlayr(L))
          if (frac_layer > 0.5) then
            DD_layer = L
          else
            DD_layer = L-1
          endif
          exit
        else
          DD_layer = 1
          exit
        endif
      enddo
      if (Denit_depth > DS(nlayr)) DD_layer = nlayr

!***********************************************************************
!***********************************************************************
!     DAILY RATE CALCULATIONS 
!***********************************************************************
      ELSEIF (DYNAMIC .EQ. RATE) THEN
!     ------------------------------------------------------------------
      IF (INDEX('N',ISWNIT) > 0) RETURN

      BD = SOILPROP % BD
      DUL = SOILPROP % DUL
      KG2PPM = SOILPROP % KG2PPM
      NLAYR = SOILPROP % NLAYR
      
!     ------------------------------------------------------------------
!     Diffusivity rate calculation - now called from SOILNI
!     call DayCent_diffusivity(dD0, sw, soilprop)
!     ------------------------------------------------------------------

      TNOXD = 0.0
!     TN2OD = 0.0     ! PG
      TN2OdenitD = 0.0     ! PG
      TN2D  = 0.0
      wfps = n2o_data % wfps
      DENITRIF = 0.0
      N2FLUX   = 0.0
      n2odenit  = 0.0

!     ------------------------------------------------------------------
!     Loop through soil layers for rate calculations
!     ------------------------------------------------------------------
      DO L = 1, NLAYR
        
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

!       CO2ppm is labile C which is derivation of CO2 produced 
!       on any day
        if (L == 1) then
!         first soil layer includes residue layer-chp
          CO2ppm(1) = (newCO2(0)+newCO2(1))*kg2ppm(L)  
        else
          CO2ppm(L) = newCO2(L)*kg2ppm(L)
        endif
      
!       Water Filled Pore Space (WFPS) WFPS_threshold
        If (dD0(L) .GE. 0.15) then 
          WFPS_thres = 0.80
        else
          WFPS_thres = (dD0(L)*250. + 43.)/100.
        endif
        
        if(wfps(L) .LE. WFPS_thres) then
          co2_correct(L) = co2PPM(L)
        else
          if(dD0(L) .GE. 0.15) then
            a_coeff = 0.004
          else 
            a_coeff = -0.1*dD0(L) + 0.019
          endif    
          co2_correct(L)=co2PPM(L)*(1.0+a_coeff*(wfps(L)-WFPS_thres))   
          !the amount labile C is adjusted taking into account diffusion
        endif  
        
!       Compute the Carbon Dioxide effect on Denitrification fDco2, 
!       ppm N Changed CO2 effect on denitrification based on paper 
!        "General model for N2O and N2 gas emissions from  soils due to 
!        denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 
!       daycent NO3 factor
        fDno3 = (A(2) + (A(3)/PI) * atan(PI*A(4)*(no3(L)-A(1))))  
        fDno3 = max(0.0, fDno3)

!chp    fDco2 = 0.1 * co2_correct(L)**1.27    !daycent labile C factor
!       daycent labile C factor
!    Comment from Upendra - 2018-10-09
!    This will always be  > 1; 
!    even at 50 ppm fDCO2 = 16 : I'm assuming CO2 is in ppm.  
!    So it will not play any role ..
        fDco2 = 0.1 * co2_correct(L)**1.3 -  min_nitrate
        fDco2 = max(0.0, fDco2)

        fNO3fCO2 = min (fDco2, fDno3)

!     9/28/2017 Based on DayCent dentitrificaion routine (denitrify.c)
!        Dtotflux = (fDno3 < fDco2) ? fDno3 : fDco2;
!        /* Minimum value for potential denitrification in top 2 soil 
!        layers */
!        /* ppm N, 9/18/00 -cindyk */
! chp: top 2 layers in DayCent = 30 cm
!        if (ilyr < 2) {
!          Dtotflux = max(0.066f, Dtotflux);
!        }
        if (L <= DD_layer) then
          fNO3fCO2 = max(fNO3fCO2, 0.066)
        endif

!       Compute wfps effect on denitrification, (fDwfps, 0-1)
!       Changed wfps effect on denitrification based on paper 
!        "General model for N2O and N2 gas emissions from soils 
!        due to denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00

        M = min(0.113, dD0(L)) * (-1.25) + 0.145
       
!       The x_inflection calculation should take into 
!        account the corrected CO2 concentration, cak - 07/31/02 

!       daycent  X_inflection/adjustment for WFPS response
        x_inflect = (9.0 - M * co2_correct(L))    
      
!       daycent WFPS factor
        fDwfps=0.45+(atan(0.6* PI *(10.0*wfps(L)- x_inflect)))/ PI   
         
        fDwfps = max(0.0, fDwfps)

        denitrifppm(L) = fDwfps * fNO3fCO2   !daycent
C       denitrifppm(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT 
!       from DSSAT DENIT version, moved here from below PG
        
C       Convert total dentrification, N2O and N2 to kg/ha/d from ppm
        denitrif(L) = denitrifppm(L)/kg2ppm(L)

! *** Moved this section up from below - needs to be done prior 
!        to partitioning to N2 and N2O
!       Modify Denitrification rate based on available NO3 
!        & flooding effects 
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

!       chp/us 4/21/2006, mod 4/26/2019
!       IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
        IF (FLOOD .GT. 0.0 .AND. fDwfps > 0.0) THEN
!          DENITRIF(L) = SNO3_AVAIL
!         chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!         previously removed 100% NO3/d
          DENITRIF(L) = SNO3_AVAIL * 0.5
        ENDIF

!       chp 4/20/2004   DENITRIF = AMAX1 (DENITRIF, DNFRATE)
        DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)  
! *** End of Moved section
        
! *** Ratio of N2 to N2O        
!       Nitrate effect on the ratio of N2 to N2O. 
!       Two approaches, use maximum of ratio1 and ratio2
!       Maximum N2/N2O ratio soil respiration function 
!       Changed the NO3 and CO2 effect on the N2/N2O ratio based 
!        on paper "General model for N2O and N2 gas emissions 
!       from soils due to denitrification"
!       Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 
!       fRno3_co2 estimates the ratio as a function of 
!        electron donor to substrate -mdh 5/17/00

        k1 = max(1.5, 38.4 - 350. * dD0(L))

        if (CO2ppm(L) > 1.E-3) then
          fRno3_co2 = max(0.16 * k1, 
     &      k1 * exp(-0.8 * no3(L)/co2_correct(L)))
!DayCent    k1 * exp(-0.8 * no3(L)/CO2ppm(L)))
        else
          fRno3_co2 = 0.16 * k1
        endif

C       WFPS effect on the N2/N2O Ratio */
C       Changed wfps effect on the N2/N2O ratio based on paper 
!       "General model for N2O and N2 gas emissions from soils 
!       due to denitrification"
C       Del Grosso et. al, GBC   12/00,  -mdh 5/16/00

        fRwfps = max(0.1, 0.015 * wfps(L)*100 - 0.32)
      
C       Compute the N2:N2O Ratio
!       Rn2n2o = max(0.1,fRno3_co2 * fRwfps)
        ratio1(L) = max(0.1, fRno3_co2 * fRwfps)
        
!       Count the number of days that water filled pore space 
!       is above 0.80     
        if (wfps(L) >= 0.80) then
            ndays_wet(L) = min(7, ndays_wet(L) + 1)
        else
            ndays_wet(L) = 0
        endif
        
!       modify Rn2n2o based on number of wet days 
        if (ndays_wet(L) > 0) then
            ratio2(L) = -330. + 334 * wfps(L) + 18.4 * ndays_wet(L)
            ratio2(L) = max(ratio2(L),0.0)
        else
            ratio2(L) = 0.0
        endif
        
C       Calculate N2O       
!       PG changed n2ofluxppm to n2odenitppm to differentiate n2o 
!       from denitrification
!       n2ofluxppm(L) = denitrifppm(L) / (Rn2n2o + 1.0)
        Rn2n2o(L) = max(ratio1(L), ratio2(L)) 
        n2odenit(L) = denitrif(L) / (Rn2n2o(L) + 1.0)

!       PG changed n2oflux to n2odenit to differentiate n2o 
!       from denitrification       
        n2flux(L) = denitrif(L) - n2odenit(L)
      
!       Reduce soil NO3 by the amount denitrified and add this to
!       the NOx pool
        DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
        CNOX       = CNOX       + DENITRIF(L)
        TNOXD      = TNOXD      + DENITRIF(L)
!       need to differentiate N2O from denitrification        
        CN2Odenit  = CN2Odenit  + n2odenit(L)         ! PG added
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


