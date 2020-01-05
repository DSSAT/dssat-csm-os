    
    subroutine SurfaceSoilTemp(estimated_lai,radn,maxt,mint,es,temp0,dlayer_cm,sw)
    
    !Soil surface temperature
    !Adapted from APSIM source code at July-2017
    !Murilo Vianna: murilodsv@gmail.br    
    
    !Use VarDefs
    implicit none
    include 'Constants.fi'
    
    !Input variables
    
    real estimated_lai              ! estimated lai from total residue and crop canopy cover
    real radn                       ! incident solar radiation (Mj/m^2)
    real maxt                       ! maximum air temperature (oC)
    real mint                       ! minimum air temperature (oC)
    real es                         ! soil evaporation (mm)
    real temp0                      ! soil surface temperature (oC)
    real dlayer_cm(max_sl)          ! thickness of soil layer I (cm) - slthickness() INPUT for soilT
    real sw(max_sl)                 ! soil water content of layer L (m3/m3) - swc() INPUT for soilT
    
    !State variables
    real A1        					! area of soil surface elements (m2/m2)
    real A2        					! area of mulch elements (both sides) (m2/m2)
    real RB        					! incoming solar beam shortwave radiation flux (W/m2)
    real RD        					! incoming diffuse shortwave radiation flux (W/m2)
    real RL        					! incoming longwave radiation flux (W/m2)
    real PSI       					! the angle between the solar beam and the vertical (rad)
    real TR        					! air temperature at reference height (K, oC)
    real ttav      					! average of idealised sinusoidal soil surface temperature (K, oC)
    real HV        					! latent heat flux up from soil surface due to water vapour (W/m2)
    real tt        					! time after midday (s)
    real RB0       					! RB at t=0
    real P0        					! P(0)
    real PPSI      					! proportion of radiation from direction PSI which penetrates mulch without interception (-)
    real SF2       					! proportion of intercepted shortwave radiation scattered forward by the mulch (-)
    real SB2       					! proportion of intercepted shortwave radiation scattered backward by the mulch (-)
    real SB1       					! proportion of intercepted shortwave radiation scattered backward by the soil surface (-)
    real ALPHA1    					! coefficient in the equations for thermal resistance RC1 to heat transfer by free convection from soil surface and mulch (W/m2/K(4/3)
    real BETA2     					! coefficient in the equations for thermal resistance RC2 to heat transfer by free convection from soil surface and mulch (W/m2/K(5/4)
    real IPSI      					! proportion of radiation from direction PSI intercepted by mulch (-)
    real PAV       					! proportion of diffuse radiation which penetrates the mulch without interception (-)
    real IAV       					! proportion of diffuse radiation intercepted by mulch (-)
    real RHO1      					! reflection coefficient of soil surface for diffuse shortwave radiation (-)
    real RHO2      					! reflection coefficient of mulch for diffuse shortwave radiation (-)
    real TAU2      					! transmission coefficient of mulch for diffuse shortwave radiation (-)
    real B1        					! unintercepted solar beam shortwave radiation flux just above soil surface (W/m2)
    real ED1       					! downward diffuse shortwave radiation flux just above soil surface (W/m2)
    real EU1       					! upward diffuse shortwave radiation fluxes just above soil durface (W/m2)
    real EU2       					! upward diffuse shortwave radiation fluxes just above mulch (W/m2)
    real ES1       					! net shortwave radiation fluxes absorbed by soil surface (W/m2)
    real ES2       					! net shortwave radiation fluxes absorbed by mulch (W/m2)
    real EL1       					! net longwave radiation fluxes absorbed by soil surface (W/m2)
    real EL2       					! net longwave radiation fluxes absorbed by mulch (W/m2)
    real HS1       					! sensible heat fluxes from soil surface to air due to convection (W/m2)
    real HS2       					! sensible heat fluxes from mulch to air due to convection (W/m2)
    real RC1       					! soil surface canopy average boundary layer resistances to sensible heat transfer (K m2/W)
    real RC2       					! mulch canopy average boundary layer resistances to sensible heat transfer (K m2/W)
    real TC1       					! soil surface temperature (K, oC)
    real TC2       					! mulch temperature (K, oC)
    real Gg        					! heat flux into the soil (W/m2)
    real PHI       					! phase angle of idealized sinusoidal soil surface temperature (rad)
    real H1        					! net heat fluxes at soil surface (W/m2)
    real H2        					! net heat fluxes at mulch (W/m2)
    real J11       					! partial derivative (W/m2/K)
    real J12       					! partial derivative (W/m2/K)
    real J21       					! partial derivative (W/m2/K)
    real J22       					! partial derivative (W/m2/K)
    real DJ        					! determinant of matrix (W2/m4/K2)
    real DTC1      					!
    real DTC2						 
    real YEFF      					!
    real F1        					! auxiliary variables ()
    real F2        					! auxiliary variables ()
    real F         					! auxiliary variables ()
    real x1							 
    real x2							 
    real x3							 
    real x4							 
    real wattm2    					! incoming shortwave radiation (W/m2)
    real tempm     					! average temperature (oC)
    real ys(max_sl+1)               ! magnitude of soil thermal admittance (W/m2/K)
    real phis(max_sl+1)             ! phase angle of soil thermal admittance (rad)
    real ratio_G(max_sl+1)          ! ratio of heat flux between top and bottom layer boundaries ()
    real ratio_T(max_sl+1)          ! ratio of temperature between top and bottom layer boundaries ()
    integer    node
    integer counter
    
    !--- Constant Values (consider to add in "Constants.fi")

    real OMEGA                       ! daily angular frequency (rad/s)
    parameter (OMEGA = 2.0*PI / 86400.0)

    real TZ                          ! temperature freezing point (K)
    parameter (TZ = 273.16D0)

    real w2                          ! average width of mulch elements (m)
    parameter (w2 = 0.02D0)
    
    !--- Implementation Section ----------------------------------
      
      node = 1
      A1 = 1.0
      A2 = 2.*estimated_lai
      
      radn = radn / 0.04186                ! convert to langleys
      
      wattm2 = 0.30d0*radn / 180.d0*698.d0
      rb0 = 0.85*wattm2
      rd = 0.15*wattm2
      ! rl calculated from Monteith 1973
      rl = 208.d0 + 6.d0*maxt
      p0 = exp(-A2 / 4.D0)
      if (p0 .eq. 1.) p0 = 0.999
      pav = exp(-A2 / 2.6D0)
      if (pav .eq. 1.) pav = 0.999

      TR = maxt + TZ
      tempm = (maxt + mint) / 2.
      ttav = tempm + p0*(maxt - tempm) + tz
      hv = 27.78*es

      sf2 = 0.22
      sb2 = 0.20
      sb1 = 0.20
      alpha1 = 1.70 * A1
      beta2 = 1.4*A2 / w2**0.25
      if (beta2 .le. 22.) beta2 = 22.

      call soilt (ys, phis, ratio_G, ratio_T,dlayer_cm,sw)
        !  DERIVE OTHER PARAMETERS
      IAV = 1.0 - PAV
      RHO1 = SB1
      RHO2 = IAV*SB2
      TAU2 = PAV + IAV*SF2
      YEFF = ys(node)*COS(phis(node))

      tt = 0.
    
 20   CONTINUE
       ! CALCULATE VARIABLES

      PHI = OMEGA*tt
      RB = RB0*COS(PHI)
      PSI = PHI
      PPSI = P0**(1. / COS(PSI))
      IPSI = 1. - PPSI
      B1 = PPSI*RB
      ED1 = (TAU2*RD + RHO2*SB1*B1 + SF2*IPSI*RB) / (1.0 - RHO1*RHO2)
      EU1 = RHO1*ED1 + SB1*B1
      EU2 = TAU2*EU1 + RHO2*RD + SB2*IPSI*RB
      ES1 = B1 + ED1 - EU1
      ES2 = RB - B1 + RD - ED1 + EU1 - EU2
      TC1 = TR
      TC2 = TR
      DTC1 = 5.0
      DTC2 = 5.0     
      
 30   CONTINUE
         !  START NEXT ITERATION
      TC1 = TC1 + DTC1
      TC2 = TC2 + DTC2
      X1 = SIGMA*TC1**4
      X2 = SIGMA*TC2**4
      EL1 = PAV*RL + IAV*X2 - X1
      EL2 = IAV*RL - 2.0*IAV*X2 + IAV*X1
      RC1 = 1.0 / (ALPHA1*(ABS(TC1 - TR))**0.333333)
      RC2 = 1.0 / (BETA2*(ABS(TC2 - TR))**0.25)
      HS1 = (TC1 - TR) / RC1
      HS2 = (TC2 - TR) / RC2

      Gg = (TC1 - ttav)*YEFF
      H1 = ES1 + EL1 - HV - HS1 - Gg
      H2 = ES2 + EL2 - HS2
      J11 = -4.0*X1 / TC1 - 1.333333 / RC1 - YEFF
      J12 = 4.0*IAV*X2 / TC2
      J21 = 4.0*IAV*X1 / TC1
      J22 = -8.0*IAV*X2 / TC2 - 1.25 / RC2
      DJ = J11*J22 - J12*J21
      DTC1 = (-J22*H1 + J12*H2) / DJ
      DTC2 = (J21*H1 - J11*H2) / DJ
            
      IF (ABS(H1) + ABS(H2) .GT. 0.01) GO TO 30

      X3 = PPSI*LOG(PPSI)
      F1 = (1. - SB1)*(PPSI + SF2*IPSI - X3*(1. - SF2))
      F2 = (1. - SB2 - SF2)*(IPSI + IAV*SB1*PPSI + X3*(1. - IAV*SB1))
      F = ys(node)*SIN(phis(node)) / (RB*(F1 - (J12 / J22)*F2))

      PHI = ATAN((TC1 - ttav)*F)

      X4 = tt
      tt = PHI / OMEGA
      IF (ABS(tt - X4) .GT. 0.1) GO TO 20

      temp0 = tc1 - tz

!c      write (6,630) iyr,jdate,g%temp0,wattm2,rb0,rd,rl,p0,pav,g%estimated_lai,tr,
!c     1     ttav,hv,g%es,beta2,ys,phis
!c  630 format(i3,i4,f5.1,4f5.0,2f5.2,3f6.0,f5.0,3f5.1,f6.3)

      !g%st_max(1) = tc1 - tz
      !do node = 2, g%num_layers + 1
      !   tc1 = tc1 / ratio_T(node-1)
      !   g%st_max(node) = tc1 - tz
      !end do
    
    end subroutine SurfaceSoilTemp
    
