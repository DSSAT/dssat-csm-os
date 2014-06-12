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

      SUBROUTINE Denit_DayCent (DYNAMIC, ISWNIT, NSWITCH, 
     &    BD, DUL, KG2PPM, newCO2, NO3, SAT, SW     !Input
     &    DENITRIF, DLTSNO3)                        !Output

!-----------------------------------------------------------------------
      USE ModuleDefs 
      USE ModuleData
      IMPLICIT  NONE
      SAVE
!-----------------------------------------------------------------------
      CHARACTER*1 ISWNIT

!      LOGICAL IUON

      INTEGER DOY, DYNAMIC, L
      INTEGER NLAYR
      !INTEGER NSOURCE, YEAR, YRDOY   

      REAL ARNTRF, FLOOD, TNOX, TNOXD, XMIN
      REAL WFDENIT
      REAL DENITRIF(NL), NITRIF(NL), SNO3_AVAIL

      REAL DLTSNO3(NL)   
      REAL BD(NL), DUL(NL)
      REAL KG2PPM(NL) 
      REAL NO3(NL), SAT(NL)
      REAL SNO3(NL), SW(NL)
      
!!!!! daycent variables  PG
      
      REAL wfps(nl), poros(nl), n2oflux(nl), n2flux(nl)
      REAL wfps_fc(nl), co2_correct(nl), co2PPM(nl)
      REAL n2ofluxppm(nl), denitrifppm(nl)    !, n2fluxppm(nl)
      REAL a_coeff, wfps_thres, fDno3, Rn2n2O, fRwfps
      REAL fRno3_CO2, k1, dD0_fc, fDCO2, fDwfps, X_inflect
      REAL m, fNo3fCo2
      REAL TN2OD, TN2O
      REAL newCO2(nl)
      REAL n2onitrif(nl)  !pn2onitrif, 
      real A(4)
      real RWC

      INTEGER NSWITCH

      
            
!***********************************************************************
!***********************************************************************
!     Seasonal initialization - run once per season
!***********************************************************************
      IF (DYNAMIC .EQ. SEASINIT) THEN
!     ------------------------------------------------------------------
!       Today's values
!       Seasonal cumulative vaules
        TNOX   = 0.0    !denitrification
        TN2O   = 0.0    ! N2O added        PG

        POROS(L)  = 1.0 - BD(L) / 2.65
        wfps(L) = sw(L) / poros(L)
        wfps_fc(L) = dul(L) / poros(L)

      open (unit=9,file='output.dat', status='unknown')
      write (9,*)' doy  L    sw     bd     poros    wfps   wfpsfc  wfpst
     &h   CO2_cor newCO2  CO2ppm a_coef denit   n2oflux  nitrif    arntr
     &f   n2onit   totn2o'

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

      DO L = 1, NLAYR
!       following code is Rolston pdf document
        RWC = SW(L)/SAT(L)                                  
        if (RWC .GE. 0.8 .AND. RWC. LE .0.9) then
            WFDENIT = RWC * 2 - 1.6
        elseif (RWC .GT. 0.9 .AND. RWC .LE. 1.0) then
            WFDENIT = RWC * 8 -7
        else
            WFDENIT = 0.0
        endif    
            
        WFDENIT = AMAX1 (AMIN1 (WFDENIT, 1.), 0.)
        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!          
!     Daycent denitrification routines PG 17/5/13      
        
      
C      call diffusivity(dD0_fc, dul, bd, wfps_fc)
C      diffusivity is a piece of Daycent code which has not been invoked in DSSAT
C      the variable dDO_fc is manually calculated for the moment
      
C      dD0_fc = 0.10    ! 0.10 for KT, fixed value since diffusivity routine is not working
C      dD0_fc = 0.16    ! 0.16 for KY, fixed value since diffusivity routine is not working
C      dD0_fc = 0.11    ! 0.11 for Canada (use KT), fixed value since diffusivity routine is not working
       dD0_fc = 0.21    ! 0.21 for India, fixed value since diffusivity routine is not working
C      dD0_fc = 0.09    ! 0.09 for France (use KT), fixed value since diffusivity routine is not working
      
      CO2ppm(L) = newCO2(L)*kg2ppm(L)  !CO2ppm is labile C which is derivation of CO2 produced on any day
      
!     Water Filled Pore Space (WFPS) WFPS_threshold
      If (dD0_fc .GE. 0.15) then 
         WFPS_thres = 0.80
      else
         WFPS_thres = (dD0_fc*250. + 43.)/100.
      endif
        
      if(wfps(L) .LE. WFPS_thres) then
         co2_correct(L) = co2PPM(L)
      else
         if(dD0_fc .GE. 0.15) then
            a_coeff = 0.004
         else 
            a_coeff = -0.1*dD0_fc + 0.019
         endif    
         co2_correct(L)=co2PPM(L)*(1.0+a_coeff*(wfps(L)-WFPS_thres))   !the amount labile C is adjusted taking into account diffusion
      endif  
  
!     Compute the Nitrate effect on Denitrification
!     Changed NO3 effect on denitrification based on paper  "General model for N2O and N2 gas emissions from soils due to denitrification"
!     Del Grosso et. al, GBC   12/00,  -mdh 5/16/00
        
      A(1) = 9.23
      A(2) = 1.556
      A(3) = 76.91
      A(4) = 0.00222

      fDno3 = (A(2) + (A(3)/PI) * atan(PI*A(4)*(no3(L)-A(1))))  !daycent NO3 factor

!     Compute the Carbon Dioxide effect on Denitrification fDco2, ppm N
!     Changed CO2 effect on denitrification based on paper "General model for N2O and N2 gas emissions from  soils due to denitrification"
!     Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 

      fDco2 = 0.1 * co2_correct(L)**1.27    !daycent labile C factor

      fNO3fCO2 = min (fDco2, fDno3)

!     Compute wfps effect on denitrification, (fDwfps, 0-1)
!     Changed wfps effect on denitrification based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
!     Del Grosso et. al, GBC     12/00,  -mdh 5/16/00

      M = min(0.113, dD0_fc) * (-1.25) + 0.145
       
!     The x_inflection calculation should take into account the corrected CO2 concentration, cak - 07/31/02 

      x_inflect = (9.0 - M * co2_correct(L))    ! daycent  X_inflection/adjustment for WFPS response
      
      fDwfps=0.45+(atan(0.6* 3.1415*(10.0*wfps(L)- x_inflect)))/PI   ! daycent WFPS factor
         
      fDwfps = max(0.0, fDwfps)

      denitrifppm(L) = fDwfps * fNO3fCO2   !daycent
C      denitrifppm(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT ! from DSSAT DENIT version, moved here from below PG
        
!     Nitrate effect on the ratio of N2 to N2O.  
!     Maximum N2/N2O ratio soil respiration function 
!     Changed the NO3 and CO2 effect on the N2/N2O ratio based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
!     Del Grosso et. al, GBC     12/00,  -mdh 5/16/00 
!     fRno3_co2 estimates the ratio as a function of electron donor to substrate -mdh 5/17/00

      k1 = max(1.5, 38.4 - 350. * dD0_fc)

      fRno3_co2 = max(0.16 * k1, k1 * exp(-0.8 * no3(L)/co2_correct(L)))

C WFPS effect on the N2/N2O Ratio */
C Changed wfps effect on the N2/N2O ratio based on paper "General model for N2O and N2 gas emissions from soils due to denitrification"
C Del Grosso et. al, GBC   12/00,  -mdh 5/16/00

      fRwfps = max(0.1, 0.015 * wfps(L)*100 - 0.32)
      
C Compute the N2:N2O Ratio

      Rn2n2o = max(0.1,fRno3_co2 * fRwfps)
      
C Calculate N2O       

      n2ofluxppm(L) = denitrifppm(L) / (Rn2n2o + 1.0)
            
C Convert total dentrification, N2O and N2 to kg/ha/d from ppm
      denitrif(L) = denitrifppm(L)/kg2ppm(L)
      n2oflux(L) = n2ofluxppm(L)/kg2ppm(L)
      n2flux(L) = denitrif(L) - n2oflux(L)
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
!         Denitrification rate
C-UPS     Corrected per e-mail 03/29/00
!         DLAG REMOVED REVISED-US 4/20/2004
!PG moved the following equation up higher when using the DSSAT version of denit        
!          DENITRIF(L) = 6.0 * 1.E-04 * CW * NO3(L) * WFDENIT * TFDENIT / KG2PPM(L)       
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
          DENITRIF(L)  = AMIN1 (DENITRIF(L), SNO3_AVAIL)

C         If flooded, lose all nitrate --------REVISED-US
!          IF (FLOOD .GT. 0.0) THEN
!            !DNFRATE = SNO3(L) - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!            DNFRATE = SNO3_AVAIL - 0.5/KG2PPM(L)        !XMIN?, SNO3_AVAIL?
!          ELSE
!            DNFRATE = 0.0
!          ENDIF

!         chp/us 4/21/2006
          IF (FLOOD .GT. 0.0 .AND. WFDENIT > 0.0) THEN
!            DENITRIF(L) = SNO3_AVAIL
!           chp 9/6/2011 remove 50% NO3/d = 97% removed in 5 days
!           previously removed 100% NO3/d
            DENITRIF(L) = SNO3_AVAIL * 0.5
          ENDIF

!chp 4/20/2004   DENITRIF = AMAX1 (DENITRIF, DNFRATE)
          DENITRIF(L) = AMAX1 (DENITRIF(L), 0.0)
          IF (NSWITCH .EQ. 6) THEN
            DENITRIF(L) = 0.0
          ENDIF

!         Reduce soil NO3 by the amount denitrified and add this to
!         the NOx pool
          DLTSNO3(L) = DLTSNO3(L) - DENITRIF(L)
          TNOX       = TNOX       + DENITRIF(L)
          TNOXD      = TNOXD      + DENITRIF(L)
          TN2O = TN2O + N2OFLUX(L)           ! PG added
          TN2OD = TN2OD + N2OFLUX(L)         ! PG added
C        ELSE
!         IF SW, ST OR NO3 FALL BELOW CRITICAL IN ANY LAYER RESET LAG EFFECT.
          !DLAG(L) = 0      !REVISED-US
C        ENDIF   !End of IF block on denitrification.
C The following code was included by PG to accumulate the gas loss from N2O and N2, which is restricted to 0-20 cm
C it also includes some diagnostics
      if (L .eq. 3) then
      open (unit=9, file='output.dat', status='unknown')
      
      write (9, 200) doy, L, sw(L), bd(L), poros(L), wfps(L),
     &wfps_fc(L), wfps_thres, CO2_correct(L), newCO2(L),CO2ppm(L),
     &a_coeff,(denitrif(1)+denitrif(2)+denitrif(3))*1000,
     &(n2oflux(1)+n2oflux(2)+n2oflux(3))*1000,
     &(nitrif(1)+nitrif(2)+nitrif(3))*1000,arntrf*1000,
     &(n2onitrif(1)+n2onitrif(2)+n2onitrif(3))*1000,
     &(n2oflux(1)+n2oflux(2)+n2oflux(3)+n2onitrif(1)+n2onitrif(2)+n2onit
     &rif(3))*1000, wfdenit, fdwfps, fdno3, x_inflect, rn2n2o
     
  200 format (1x,i3,1x,i3,12(f8.3),9(f9.3))
  
      endif

      END DO   !End of soil layer loop.

      CALL PUT('NITR','TNOXD',ARNTRF) 
      CALL PUT('NITR','TN2OD',TN2OD)

!***********************************************************************
!***********************************************************************
!     END OF DYNAMIC IF CONSTRUCT
!***********************************************************************
      ENDIF

C-----------------------------------------------------------------------
  
      RETURN
      END SUBROUTINE Denit_DayCent

!=======================================================================
! Denit_DayCent Variables 
!-----------------------------------------------------------------------

!***********************************************************************
