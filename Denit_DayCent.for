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
     &    CNOX, TNOXD, N2O_data,                      !Output
!         Temp input for Output.dat file:
     &    NITRIF, TNITRIFY, n2onitrif)                !Temp

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
      
      REAL wfps(nl), poros(nl)
      REAL wfps_fc(nl), co2_correct(nl), co2PPM(nl)
      REAL a_coeff, wfps_thres, fDno3, Rn2n2O, fRwfps
      REAL fRno3_CO2, k1, fDCO2, fDwfps, X_inflect
      REAL m, fNo3fCo2
      REAL newCO2(nl), dD0_fc(nl)
      real A(4)
!     real RWC

      TYPE (N2O_type) N2O_DATA
!          Cumul Daily  Layer ppm        Layer kg
      REAL CNOX, TNOXD, denitrifppm(NL), DENITRIF(NL)  !Denitrification
      REAL CN2,  TN2D,                   n2flux(nl)    !N2
      REAL CN2O, TN2OD, n2ofluxppm(NL),  n2oflux(nl)   !N2O 

!     Temp variables for Output.dat file:
      REAL TNITRIFY, NITRIF(NL), n2onitrif(NL)

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
!       Today's values
!       Seasonal cumulative vaules
        CNOX   = 0.0    !denitrification
        CN2O   = 0.0    ! N2O added        PG
        CN2    = 0.0    ! N2

        DO L = 1, NLAYR
          POROS(L)  = 1.0 - BD(L) / 2.65
          wfps_fc(L) = dul(L) / poros(L)
          wfps(L) = sw(L) / poros(L)
        ENDDO

!     Function for diffusivity
      call DayCent_diffusivity(dD0_fc, DUL, BD)

!     Compute the Nitrate effect on Denitrification
!     Changed NO3 effect on denitrification based on paper  "General model for N2O and N2 gas emissions from soils due to denitrification"
!     Del Grosso et. al, GBC   12/00,  -mdh 5/16/00
      A(1) = 9.23
      A(2) = 1.556
      A(3) = 76.91
      A(4) = 0.00222

      open (unit=9,file='output.dat', status='unknown')
      write (9,'(A)')' doy  L    sw     bd     poros    wfps   wfpsfc  w
     &fpsth   CO2_cor newCO2  CO2ppm a_coef denit   n2oflux  nitrif  Tni
     &trify   n2onit   totn2o'

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
      TN2OD = 0.0     ! PG
      TN2D  = 0.0

      DO L = 1, NLAYR
        wfps(L) = sw(L) / poros(L)

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

        CO2ppm(L) = newCO2(L)*kg2ppm(L)  !CO2ppm is labile C which is derivation of CO2 produced on any day
      
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
        
!       Nitrate effect on the ratio of N2 to N2O.  
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

        Rn2n2o = max(0.1,fRno3_co2 * fRwfps)
      
C       Calculate N2O       

        n2ofluxppm(L) = denitrifppm(L) / (Rn2n2o + 1.0)
            
C       Convert total dentrification, N2O and N2 to kg/ha/d from ppm
        denitrif(L) = denitrifppm(L)/kg2ppm(L)
        n2oflux(L) = n2ofluxppm(L)/kg2ppm(L)
        n2flux(L) = denitrif(L) - n2oflux(L)
      
!******************************************************************************
!       CHP 6/15/2014 Note: if DENITRIF is modified below, need to go back 
!         and re-proportion fluxes.
!******************************************************************************

!       Denitrification rate
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

!       Reduce soil NO3 by the amount denitrified and add this to
!       the NOx pool
        DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
        CNOX       = CNOX       + DENITRIF(L)
        TNOXD      = TNOXD      + DENITRIF(L)
        CN2O = CN2O + N2OFLUX(L)           ! PG added
        TN2OD = TN2OD + N2OFLUX(L)         ! PG added

        CN2  = CN2  + N2FLUX(L)            ! PG
        TN2D = TN2D + N2FLUX(L)            ! PG

C The following code was included by PG to accumulate the gas loss from N2O and N2, which is restricted to 0-20 cm
C it also includes some diagnostics
        if (L .eq. 3) then
          open (unit=9, file='output.dat', status='unknown')
      
          write (9, 200) doy, L, sw(L), bd(L), poros(L), wfps(L),
     &wfps_fc(L), wfps_thres, CO2_correct(L), newCO2(L),CO2ppm(L),
     &a_coeff,(denitrif(1)+denitrif(2)+denitrif(3))*1000,
     &(n2oflux(1)+n2oflux(2)+n2oflux(3))*1000,
     &nitrif(1),TNITRIFY,
     &(n2onitrif(1)+n2onitrif(2)+n2onitrif(3))*1000,
     &(n2oflux(1)+n2oflux(2)+n2oflux(3)+n2onitrif(1)+n2onitrif(2)+n2onit
     &rif(3))*1000, wfdenit, fdwfps, fdno3, x_inflect, rn2n2o
     
  200 format (1x,i3,1x,i3,12(f8.3),9(f9.3))

        endif

      END DO   !End of soil layer loop.

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF
!-----------------------------------------------------------------------
      N2O_data % CN2      = CN2
      N2O_data % CN2O     = CN2O
      N2O_data % CNOX     = CNOX
      N2O_data % TN2D     = TN2D
      N2O_data % TN2OD    = TN2OD
      N2O_data % TNOXD    = TNOXD
      N2O_data % DENITRIF = DENITRIF
      N2O_data % N2OFLUX  = N2OFLUX
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
      subroutine DayCent_diffusivity(dD0_fc, dul, bd)      
!      the variable dDO_fc is manually calculated for the moment

      use ModuleDefs
      real, dimension(NL), intent(in) :: dul, bd
      real, dimension(NL), intent(out):: dD0_fc
      integer L
      
! From Iurii Shcherbak 6 July 2014
! Linear model
! Dfc = 0.741804 - 0.287059 BD - 0.755057 FC
! Adj R^2 = 0.999412
!  
! Quadratic
! Dfc = 0.906277 - 0.461712 BD + 0.045703 BD^2 - 1.19827 FC +  0.225558 BD FC + 0.292491 FC^2
! Adj R^2 = 0.999997

      do L = 1, NL
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
