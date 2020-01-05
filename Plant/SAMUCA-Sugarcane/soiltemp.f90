subroutine soiltemp(task)
    
    !Soil temperature
    !Adapted from SWAP including surface boudary condition (Ts)
    !created in 2004/ adapted in 2017
    !Modifications (Murilo Vianna):
    ! - node and layers thickness equal
    ! - empirical relation of AirT and LAI to estimate surface temperature
    ! - theta(i) = swc(i)
    
    Use VarDefs
    Implicit None
    
   ! --- local
      integer task
      integer i,lay, ierror
      integer step,nstep
      integer day
      integer sl
      
      real  tmpold(macp),tab(mabbc*2),dummy,afgen,gmineral
      real  thoma(macp),thomb(macp),thomc(macp),thomf(macp)
      real  theave(macp),heacap(macp),heacnd(macp),heacon(macp)
      real  heaconbot,qhbot
      real  apar, dzsnw, heaconsnw, Rosnw
      real estimated_lai
      real htora
      real croph
      real sat_point(max_sl)
      real tetha(max_sl)
      real avgtmn(365)
      real lai_red
      real G(max_sl)    ! Soil heat flux (W m-2)
      real heacon_mu_dry
      real heacon_mu_dwet
      real G_soil_surf
      real muwat_frac
      real rhomulch_layer
      real musat_frac
      real muair_frac
      real muorg_frac
      
      character messag*200
      
      !--- Specific Heat Capacity (J/kg/K)
      real  cQuartz,cClay,cWat,cAir,cOrg
      Parameter (cQuartz = 800.0d0, cClay = 900.0d0, cWat = 4180.0d0, cAir = 1010.0d0, cOrg = 1920.0d0)
      
      !--- Density (kg/m3)
      real dQuartz, dClay, dWat, dAir, dOrg
      Parameter (dQuartz = 2660.0d0, dClay = 2650.0d0, dWat = 1000.0d0, dAir = 1.2d0, dOrg = 1300.0d0)
            
      !--- Minimum number of steps for accurate numerical simulations (2 hrs)
      integer :: min_steps = 12 
    save
    
        
    goto (1000,2000,3000) task
    
    
1000 continue
         
     !Initialization
     
    ! ----------------------------------------------------    
    nheat   = nlay
    do sl = 1, nheat
        tsoil(sl)   = 20.d0
        zh(sl)      = -(dep(sl) - slthickness(sl) / 2.d0 )
    enddo
    
    sat_point = sat !Arrays   
       
    ! ------------------------------------------------------      
    ! ---   numerical solution, use initial specified soil temperatures ---       
        do i = 1, nheat
            tab(i*2) = tsoil(i)
            tab(i*2-1) = abs(zh(i) - hmulch) !Mulch thickness added when is necessary
        end do    
    
        !Considering number of node and layers thickness equal
        !Mulch blanket effect on soil temperature (Adding mulch cover node)
        if(mulcheffect .and. mulchcover)then
            
            numnod          = nlay + 1 !Mulch blanket considered as one node/layer
            !Nodes profile with mulch blanket
            do i = 1, numnod                
                if(i .eq. 1)then
                    dz(i) = hmulch
                else
                    dz(i) = slthickness(i-1)
                endif               
            enddo           
        else                
            numnod          = nlay
            dz(1:nlay)      = slthickness(1:nlay)        
        endif
        
        znod(macp) = 0.d0        
        do i = 1, numnod            
            !znod(i) = (upper(i) + slthickness(i) / 2.) * -1.            
            if(i .eq. 1) then                                 
                znod(i)     = dz(i) / 2. * -1.
                disnod(i)   = - znod(i)                
            else
                znod(i)     = ((dz(i-1) / 2.) - znod(i-1) + dz(i) / 2.) * -1.
                disnod(i)   = znod(i-1) - znod(i)        
            endif            
        enddo
        
        do i = 1, numnod
            tsoil(i) = afgen(tab,macp*2,abs(znod(i)))          
        end do
    
        ! ---   initialize dry bulk density and volume fractions sand, clay and organic matter
        if(mulcheffect .and. mulchcover)then      
        
            !Mulch blancket properties (Set to zero for simplicity)
            fquartz(1)  = 0.d0
            fclay(1)    = 0.d0
            forg(1)     = 0.3716 !optimized
            
            sat_point(2:numnod) = sat(1:nlay)
            sat_point(1)        = 0.2 !Arbitrary for mulch...Its is possible to calculate with dwsat and mulch density
            
            do i = 2, numnod
                !lay = layer(i) Not used
                dummy = orgmat(i-1)/(1.0d0 - orgmat(i-1))
                gmineral = (1.0d0 - sat_point(i)) / (0.370d0 + 0.714d0*dummy)
                fquartz(i) = (psand(i-1) + psilt(i-1))*gmineral/2.7d0
                fclay(i) = pclay(i-1)*gmineral/2.7d0
                forg(i) = dummy*gmineral/1.4d0
            end do
                   
        else
        
            sat_point = sat
            
            do i = 1, numnod
                !lay = layer(i) Not used
                dummy = orgmat(i)/(1.0d0 - orgmat(i))
                gmineral = (1.0d0 - sat_point(i)) / (0.370d0 + 0.714d0*dummy)
                fquartz(i) = (psand(i) + psilt(i))*gmineral/2.7d0
                fclay(i) = pclay(i)*gmineral/2.7d0
                forg(i) = dummy*gmineral/1.4d0
            end do
            
        endif        
                
     !--- write output file header     
     write (outst,10) 'Simulations for: ', trim(prjname)
     write (outst,11)
     
     if (numnod.le.9) then
         write (outst,12) (i,i=1,numnod)
     else
         
         write (outst,13) (i,i=1,9), (i,i=10,numnod)
     endif     
     
     
10   format(2A,/) 	   
11   format('Results of soil temperature simulation in daily output:')
12   format('*',/,'Year, Doy, Das, Dap, Tmed, Tetop, Mulch',9(',    T',i1),', TeBot, G')
13   format('*',/,'Year, Doy, Das, Dap, Tmed, Tetop, Mulch',9(',    T',i1),1024(',   T',i2),', TeBot, G')
        
     
     write (outst,'(i4,a1,i3,a1,i3,a1,i3,1024(a1,f6.1:))') year,comma,doy,comma,das,comma,dap,comma,tmn,comma,tetop,(comma,tsoil(i),i=1,numnod),comma,tebot
        
     return
2000 continue
     
     ! --- Soil temperature rate and state variables
     if(mulcheffect .and. mulchcover)then

         !--- Create the mulch cover
         alb_surface     = mualb(seqnow)     !- Surface Albedo is now Mulch Albedo
         numnod          = nlay + 1          !- Mulch blanket considered as one soil layer

         !--- Layer profile with mulch blanket
         do i = 1, numnod
             if(i .eq. 1)then
                 dz(i) = hmulch             ![cm]
             else
                 dz(i) = slthickness(i-1)   ![cm]
             endif
         enddo

         znod(macp) = 0.d0
         do i = 1, numnod
             !--- Mid point depth for each layer and distance among them [cm]
             if(i .eq. 1) then
                 znod(i)     = dz(i) / 2. * -1.
                 disnod(i)   = - znod(i)
             else
                 znod(i)     = ((dz(i-1) / 2.) - znod(i-1) + dz(i) / 2.) * -1.
                 disnod(i)   = znod(i-1) - znod(i)
             endif
         enddo

         !--- Mulch blancket properties (Set to zero for simplicity)
         fquartz(1)  = 0.d0
         fclay(1)    = 0.d0
         forg(1)     = 0.d0

         sat_point(2:numnod)= sat(1:nlay)         
         sat_point(1)       = muam(seqnow) * mumass(seqnow) / hmulch * 1.e-5 ![cm3[H2O] cm-3 [Mulch]]
         
         do i = 2, numnod
             dummy = orgmat(i-1)/(1.0d0 - orgmat(i-1))
             gmineral = (1.0d0 - sat_point(i)) / (0.370d0 + 0.714d0*dummy)
             fquartz(i) = (psand(i-1) + psilt(i-1))*gmineral/2.7d0
             fclay(i) = pclay(i-1)*gmineral/2.7d0
             forg(i) = dummy*gmineral/1.4d0
         enddo

     else

         alb_surface     = albedo(1)
         numnod          = nlay
         dz(1:nlay)      = slthickness(1:nlay)

         znod(macp) = 0.d0
         do i = 1, numnod
             !znod(i) = (upper(i) + slthickness(i) / 2.) * -1.
             if(i .eq. 1) then
                 znod(i)     = dz(i) / 2. * -1.
                 disnod(i)   = - znod(i)
             else
                 znod(i)     = ((dz(i-1) / 2.) - znod(i-1) + dz(i) / 2.) * -1.
                 disnod(i)   = znod(i-1) - znod(i)
             endif
         enddo

         sat_point = sat

         do i = 1, numnod
             !lay = layer(i) Not used
             dummy = orgmat(i)/(1.0d0 - orgmat(i))
             gmineral = (1.0d0 - sat_point(i)) / (0.370d0 + 0.714d0*dummy)
             fquartz(i) = (psand(i) + psilt(i))*gmineral/2.7d0
             fclay(i) = pclay(i)*gmineral/2.7d0
             forg(i) = dummy*gmineral/1.4d0
         enddo

     endif
       
    !--- Surface temperature upper boundary condition
    select case (stempm(1))     
        
    case (1)
        !--- Empirical relation on Tsurface        
        if (mulcheffect .and. mulchcover)then
            !--- If mulch cover is on the field assume that mulch surface temperature is equal to daily mean temperature
            tetop = tmn !best fitted to air temperature           
        else
            !--- Impose crop shedding effect on soil surface (empirical)
            lai_red = 0.15 !-> fitted to field data
            tetop = tmin + (tmax - tmin) * exp(-lai_red * lai)  
        endif        
         
    case(2)        
        !Energy balance modified (Monteith & Unsworth) - Tested against field data
        !alb_surface changes surface radiation absortion trasnmited through the canopy
        !mulch blanket is simulated with low heat conduction to soil surface
                
        !--- Compute Solar Radiation
        call astro() !From Weather.f90
        
        Qo      = dso / 1.e6        ! MJ m-2 d-1
        croph   = pleng             ! Crop height        
        
        call SurfaceTemp_Mod(srad,Qo,tmax,tmin,alt(1),alb_surface,rh,lai,extcoef,croph,hrnc,dhrlai,tetop)        
        
    case(3)
         
        !Surface temperature method from APSIM - Tested against field data without success (lack of documentation on this approach)
        !Aparently it uses the a lai of soil cover (lai canopy + lai mulch) to simulate the radiation transmited to soil surface
         estimated_lai = .9 !0.5 is an arbitrary value for lai of the mulch  (this lai is based on fraction of soil cover)
        call SurfaceSoilTemp(estimated_lai,srad,tmax,tmin,es,tetop,slthickness,swc)
        
    end select   
    
    !--- Average water content between today and yesterday (PRT)
    !--- PRT:
    !--- This comes from SWAP whereas the "yesterday" value Hhere used is the swc of the i-1 time-step (fraction of a day)
    !--- Not sure about the implications of using such rough step (yesterday)
    if(mulcheffect .and. mulchcover)then
        theave(1) = 0.d0 !Considering no water storage in mulch cover (may add it in the future) (PRT) (PIT) -> make use of muwat
        do i = 2,numnod          
            theave(i) = 0.5d0 * (swc(i-1) + swcm1(i-1))
        enddo
    else     
        do i = 1,numnod          
            theave(i) = 0.5d0 * (swc(i) + swcm1(i)) !swcm1 = yesterday swc    
        enddo    
    endif
    
    
    if(addmulch)then
        !--- Offset temperature and saturation profile
        tsoil(2:numnod) = tsoil(1:numnod-1)
        tsoil(1)        = tetop !Initial mulch temperature as equal as soil surface
        
    elseif(remmulch)then
        !--- Offset temperature and saturation profile
        do i = 1,numnod-1
            tsoil(i) = tsoil(i+1)  
        enddo
        tsoil(numnod) = tsoil(numnod-1) !Bottom soil profile temperature set as equal as above layer before profile end        
    endif    
    
    !Assuming 48 steps per day (same as APSIM) to solve temperature (SWAP uses a dynamic step, and most of time uses the max step [default = 0.2 or nstep = 5] )
    
    nstep   = min_steps
    dt = 1. / nstep
    
    !--- Heat flow for nstep
    do step = 1, nstep   
    
     if (SwBotbHea(1).eq.1) then
    ! ---   no heat flow through bottom of profile assumed
         
         if(addmulch)then         
             TeBot = Tsoil(Numnod-1)             
         else            
             TeBot = Tsoil(Numnod)
         endif         
             
     elseif (SwBotbHea(1).eq.2) then
    ! ---   bottom temperature is computed as sinoidal curve
         
         TeBot = tbot_mean(1) + tbot_ampli(1)*(sin(0.0172d0*(doy-tbot_imref(1)+91.0d0)+(dep(Numnod)*-1.)/tbot_ddamp(1))) / exp(dep(Numnod)/tbot_ddamp(1))
         
     endif
         
     ! --- save old temperature profile
     do i = 1,numnod
          tmpold(i) = tsoil(i)    
     enddo
     
     ! --- compute heat conductivity and capacity
     ! --- calculate nodal heat capacity and thermal conductivity (using average swc between i and i-1) 
        call devries(numnod,theave,sat_point,heacap,heacnd,fquartz,fclay,forg)
                
        if(mulcheffect .and. mulchcover)then
            !--- Use Bussière and Cellier (1994) - https://doi.org/10.1016/0168-1923(94)90066-3 (Table 2)
            
            !--- Mulch thermal conductivity properties dry and wet conditions (PIT) - Make these as model parameters
            heacon_mu_dwet  = lam_dmu_wet(seqnow)
            heacon_mu_dry   = lam_mu_dry(seqnow)
            
            !--- Heat conductivity of mulch layer (J/cm/K/d)
            heacon(1)   = (heacon_mu_dry + heacon_mu_dwet * muwat) * 864.
            
            !--- Fraction of water content in mulch layer (m3/m3)
            !--- muwat in kg/m3; hmulch in cm
            muwat_frac  = (muwat / (hmulch / 1e2)) / 1e3
            
            !--- Fraction of water content in mulch layer at saturation point (m3/m3)
            musat_frac  = (musat / (hmulch / 1e2)) / 1e3
            
            !--- Update mulch layer density (kg/m3)
            rhomulch_layer = rhomulch + muwat
            
            !--- Fraction of air in mulch layer
            muair_frac  = min(1.0, 0.5d0 + (musat_frac - muwat_frac))
            
            !--- Fraction of organic material in mulch
            muorg_frac  = 1 - (muair_frac + musat_frac)
            
            !--- Heat Capacity of mulch layer (J/kg/K)
            heacap(1) = (muwat_frac*dWat*cWat + muair_frac*dAir*cAir + muorg_frac*dOrg*cOrg) * 1.e-6
            
        else
            !--- Use DeVries for Soil thermal properties
            heacon(1) = heacnd(1)
            heacap(1) = heacap(1)
        endif
        
        do i = 2,numnod
            heacon(i) = 0.5d0 * (heacnd(i) + heacnd(i-1))
        enddo
        
    ! ---   calculate new temperature profile --------------------------------

    ! ---   calculation of coefficients for node = 1
        i = 1
    ! ---   temperature fixed at soil surface
        thoma(i) = - dt * heacon(i) / (dz(i) * disnod(i))
        thomc(i) = - dt * heacon(i+1) / (dz(i) * disnod(i+1))
        thomb(i) = heacap(i) - thoma(i) - thomc(i)
        thomf(i) = heacap(i) * tmpold(i) - thoma(i) * TeTop
            
    ! ---   calculation of coefficients for 2 < node < numnod
    do i = 2,numnod-1 
          thoma(i) = - dt * heacon(i) / (dz(i) * disnod(i))
          thomc(i) = - dt * heacon(i+1) / (dz(i) * disnod(i+1))
          thomb(i) = heacap(i) - thoma(i) - thomc(i)
          thomf(i) = heacap(i) * tmpold(i)
    enddo
    
    ! ---   calculation of coefficients for node = numnod
        i = numnod
        if (SwBotbHea(1).eq.1) then
    ! ---   no heat flow through bottom of profile assumed
           qhbot = 0.0d0
           thoma(i) = - dt * heacon(i) / (dz(i) * disnod(i))
           thomb(i) = heacap(i) - thoma(i)
           thomf(i) = heacap(i) * tmpold(i) - (qhbot * dt)/dz(i)
        elseif (SwBotbHea(1).eq.2) then
    ! ---   bottom temperature is prescribed
           heaconBot = heacnd(i)
           thoma(i)  = - dt * heacon(i) / (dz(i) * disnod(i))
           thomc(i)  = - dt * heaconBot / (dz(i) * 0.5d0 * dz(i))
           thomb(i)  = heacap(i) - thoma(i) - thomc(i)
           thomf(i)  = heacap(i) * tmpold(i) - thomc(i) * TeBot
        endif

    ! ---   solve for vector tsoil a tridiagonal linear set
        call tridag (numnod, thoma, thomb, thomc, thomf, tsoil,ierror)
        if(ierror.ne.0)then
           msg = 'During a call from Soil Temperature an error occured in TriDag'
        call wmsg(4)
        end if    
    
    enddo
    
    !--- Compute Soil Heat Flux (G)
    do sl = 1, numnod       
        if(sl .eq. 1)then
            G(sl) = -heacon(sl) * (TeTop - tsoil(sl)) / znod(sl)
        elseif(sl .eq. numnod)then
            G(sl) = -heacon(sl) * (tsoil(sl) - TeBot) / znod(sl)
        else
            G(sl) = -heacon(sl) * (tsoil(sl-1) - tsoil(sl)) / znod(sl)
        endif        
    enddo
    
    !--- Convert to W/m2
    G = G * 1e4 / 86400
    
    if(mulcheffect .and. mulchcover)then
        G_soil_surf = G(2)
    else
        G_soil_surf = G(1)
    endif
        
    return
    
3000 continue
     !write in output file
     if(mulcheffect .and. mulchcover)then
         write (outst,'(i4,a1,1x,i3,a1,1x,i5,a1,1x,i3,1024(a1,1x,f6.1:))') year,comma,doy,comma,das,comma,dap,comma,tmn,comma,tetop,(comma,tsoil(i),i=1,numnod),comma,tebot,comma,G_soil_surf,comma,hmulch,comma,heacap(1),comma,heacon(1)
     else
         write (outst,'(i4,a1,1x,i3,a1,1x,i5,a1,1x,i3,1024(a1,1x,f6.1:))') year,comma,doy,comma,das,comma,dap,comma,tmn,comma,tetop,comma,-99.,(comma,tsoil(i),i=1,numnod),comma,tebot,comma,G_soil_surf,comma, 0.d0,comma,heacap(1),comma,heacon(1)
     endif
          
     return
    
end subroutine soiltemp
    