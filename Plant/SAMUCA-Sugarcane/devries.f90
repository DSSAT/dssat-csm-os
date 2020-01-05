Subroutine Devries (NumNod,swc,sat,HeaCap,HeaCon,FQUARTZ,FCLAY,FORG)
!***********************************************************************
!* Purpose:    Calculate soil heat capacity and conductivity for each  *
!*             compartment by full de Vries model                      *     
!* References:                                                         *
!* Description:                                                        *
!* de Vries model for soil heat capcity and thermal conductivity.      * 
!* Heat capacity is calculated as average of heat capacities for each  *
!* soil component. Thermal conductivity is calculated as weighted      *
!* average of conductivities for each component. If theta > 0.05 liquid*
!* water is assumed to be the main transport medium in calculating the *
!* weights. If theta < 0.02 air is assumed to be the main transport    *
!* medium (there is also an empirical adjustment to the conductivity). *
!* For 0.02 < theta < 0.05 conductivity is interpolated.               *
!* See: Heat and water transfer at the bare soil surface, H.F.M Ten    *
!* Berge (pp 48-54 and Appendix 2)                                     *
!***********************************************************************
!* Input:                                                              *
!* NumNod - number of compartments (-)                                 *
!* theta/THETAS - volumetric soil moisture/ saturated vol. s. moist (-)*
!* Fquartz, Fclay and Forg - volume fractions of sand, clay and org.ma.*
!* Output:                                                             *
!* HeaCap - heat capacity (J/m3/K)                                     *
!* HeaCon - thermal conductivity (W/m/K)                               *
!***********************************************************************
      Implicit None
      !Include 'Constants.fi'
      
!     (i) Global declarations                                          
!     (i.i) Input                                                      
      Integer NumNod
      real swc(200),sat(200)
!     (i.ii)
      real HeaCap(5000), HeaCon(5000)
!     (ii) Local declarations                                          
      Integer Node
      real kqa,kca,kwa,koa,kaa,kqw,kcw,kww,kow,kaw
      Parameter (kaa = 1.0d0, kww = 1.0d0)
      real fAir(5000),fClay(5000),fQuartz(5000),fOrg(5000)
      real HeaConDry,HeaConWet
!     (iii) Parameter declarations                                     
!     (iii.i) Physical constants                                       
!     Specific heats (J/kg/K)                                          
      real  cQuartz,cClay,cWat,cAir,cOrg
      Parameter (cQuartz = 800.0d0, cClay = 900.0d0, cWat = 4180.0d0, cAir = 1010.0d0, cOrg = 1920.0d0)
      
!     Density (kg/m3)
      real dQuartz, dClay, dWat, dAir, dOrg
      Parameter (dQuartz = 2660.0d0, dClay = 2650.0d0, dWat = 1000.0d0, dAir = 1.2d0, dOrg = 1300.0d0)
      
!     Thermal conductivities (W/m/K)                                   
      real  kQuartz,kClay,kWat,kAir,kOrg
      Parameter (kQuartz = 8.8d0, kClay = 2.92d0, kWat = 0.57d0, kAir = 0.025d0, kOrg = 0.25d0)
!     
      real  GQuartz,GClay,GWat,GAir,GOrg
      Parameter (GQuartz = 0.14d0, GClay = 0.0d0 , GWat = 0.14d0, GAir = 0.2d0, GOrg = 0.5d0)
      
!     (iii.ii) theta 0.02, 0.05 and 1.00                               
      real thetaDry,thetaWet
      Parameter (thetaDry = 0.02d0, thetaWet = 0.05d0)    
! ----------------------------------------------------------------------
!     (0) Weights for each component in conductivity calculations      
!     (Calculate these but define as parameters in later version)     
      kaw = 0.66d0 / (1.0d0 + ((kAir/kWat) - 1.0d0)     * GAir)     +0.33d0 / (1.0d0 + ((kAir/kWat)     - 1.0d0) * (1.0d0 - 2.0d0 * GAir)) 
      kqw = 0.66d0 / (1.0d0 + ((kQuartz/kWat)-1.0d0)    * GQuartz)  +0.33d0 / (1.0d0 + ((kQuartz/kWat)  - 1.0d0) * (1.0d0 - 2.0d0 * GQuartz)) 
      kcw = 0.66d0 / (1.0d0 + ((kClay/kWat) - 1.0d0)    * GClay)    +0.33d0 / (1.0d0 + ((kClay/kWat)    - 1.0d0) * (1.0d0 - 2.0d0 * GClay)) 
      kow = 0.66d0 / (1.0d0 + ((kOrg/kWat) - 1.0d0)     * GOrg)     +0.33d0 / (1.0d0 + ((kOrg/kWat)     - 1.0d0) * (1.0d0 - 2.0d0 * GOrg)) 
      kwa = 0.66d0 / (1.0d0 + ((kWat/kAir) - 1.0d0)     * GWat)     +0.33d0 / (1.0d0 + ((kWat/kAir)     - 1.0d0) * (1.0d0 - 2.0d0 * GWat)) 
      kqa = 0.66d0 / (1.0d0 + ((kQuartz/kAir)-1.0d0)    * GQuartz)  +0.33d0 / (1.0d0 + ((kQuartz/kAir)  - 1.0d0) * (1.0d0 - 2.0d0 * GQuartz)) 
      kca = 0.66d0 / (1.0d0 + ((kClay/kAir) - 1.0d0)    * GClay)    +0.33d0 / (1.0d0 + ((kClay/kAir)    - 1.0d0) * (1.0d0 - 2.0d0 * GClay)) 
      koa = 0.66d0 / (1.0d0 + ((kOrg/kAir) - 1.0d0)     * GOrg)     +0.33d0 / (1.0d0 + ((kOrg/kAir)     - 1.0d0) * (1.0d0 - 2.0d0 * GOrg)) 

      Do Node = 1,NumNod
          
!        (1) Air fraction
         fAir(Node) = sat(Node) - swc(Node)

!        (2) Heat capacity (W/m3/K) is average of heat capacities for  
!        all components (multiplied by density for correct units)      
         HeaCap(Node) = fQuartz(Node)*dQuartz*cQuartz + fClay(Node)*dClay*cClay + swc(Node)*dWat*cWat + fAir(Node)*dAir*cAir + fOrg(Node)*dOrg*cOrg

!        (3) Thermal conductivity (W/m/K) is weighted average of       
!        conductivities of all components                              
!        (3.1) Dry conditions (include empirical correction) (eq. 3.44)
         If (swc(Node).LE.thetaDry) Then
            HeaCon(Node) = 1.25d0 * (kqa*fQuartz(Node)*kQuartz + kca*fClay(Node)*kClay + kaa*fAir(Node)*kAir + koa*fOrg(Node)*kOrg + kwa*swc(Node)*kWat) / (kqa * fQuartz(Node) + kca * fClay(Node) + kaa * fAir(Node) + koa * fOrg(Node) + kwa * swc(Node))

!        (3.2) Wet conditions  (eq. 3.43)                              
         Else If (swc(Node).GE.thetaWet) Then
            HeaCon(Node) = (kqw*fQuartz(Node)*kQuartz + kcw*fClay(Node)*kClay + kaw*fAir(Node)*kAir + kow*fOrg(Node)*kOrg + kww*swc(Node)*kWat) / (kqw * fQuartz(Node) + kcw * fClay(Node) + kaw * fAir(Node) + kow * fOrg(Node) + kww * swc(Node))

!        (3.3) dry < theta < wet (interpolate)
         Else
!           (3.3.1) Conductivity for theta = 0.02                      
            HeaConDry = 1.25d0 * (kqa*fQuartz(Node)*kQuartz + kca*fClay(Node)*kClay + kaa*fAir(Node)*kAir + koa*fOrg(Node)*kOrg + kwa*thetaDry*kWat) / (kqa * fQuartz(Node) + kca * fClay(Node) + kaa * fAir(Node) + koa * fOrg(Node) + kwa * thetaDry)
!           (3.3.1) Conductivity for theta = 0.05                      
            HeaConWet = (kqw*fQuartz(Node)*kQuartz + kcw*fClay(Node)*kClay + kaw*fAir(Node)*kAir + kow*fOrg(Node)*kOrg + kww*thetaWet*kWat) / (kqw * fQuartz(Node) + kcw * fClay(Node) + kaw * fAir(Node) + kow * fOrg(Node) + kww * thetaWet)
!         (3.3.3) Interpolate                                          
          HeaCon(Node) = HeaConDry + (swc(Node)-thetaDry) * (HeaConWet - HeaConDry) / (thetaWet - thetaDry)      
         End If

! ---    conversion of capacity from J/m3/K to J/cm3/K
         HEACAP(NODE) = HEACAP(NODE)*1.0d-6

! ---    conversion of conductivity from W/m/K to J/cm/K/d
         HEACON(NODE) = HEACON(NODE)*864.0d0

      End Do

      Return
end subroutine
