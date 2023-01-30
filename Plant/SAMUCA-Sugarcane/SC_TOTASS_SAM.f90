   
!ubroutine totass(daynr,dayl,lat_sim,DEC,SNDN,SNUP,CLOUDS,ISINB,S0N,amax,eff,lai,kdif,scv,srad,par,dtga,Acanopy,Qleaf,incpar,phot_layer,frac_li)
subroutine totass(      dayl,lat_sim,DEC,SNDN,SNUP,CLOUDS,ISINB,S0N,amax,eff,lai,kdif,scv,srad,par,dtga,Acanopy,Qleaf,incpar,phot_layer,frac_li)
! ----------------------------------------------------------------------
! --- calculates daily total gross assimilation (dtga) by performing
! --- a gaussian integration over time at three different times of 
! --- the day, irradiance is computed and used to calculate the instan- 
! --- taneous canopy assimilation, whereafter integration takes place.
! --- more information on this subroutine is given by Goudriaan, J. in:
! --- "Light Distribution. In Canopy Photosynthesis: From Basics to Applications", 2016.
! ----------------------------------------------------------------------
      Implicit None    
 !     include 'constants.fi' ! (SAMUCA's Crop Modelling Shell)
      
      external HANG, HRAD, HPAR, FRACD

      integer ghour
      integer glai
      integer gl
!     integer daynr       
    
      real dlaic
      real frac_li
      real par_check
      real par_ratio
      real lai_sl_upper(5)
      real lai_sl_bottom(5)
      real lai_sl(5)
      real lai
      real kdif
      real amax
      real srad      
      real dayl
      real dtga
      real dvisabs
      real eff
!     real fgros      
      real hour
      real par
      real pardif
      real pardir
      real phot
      real phott
      real photth
      real photl
      real phos
      real phoshd
      real phosun
      real sinb      
      real Acanopy(3+1,5+1)     
      real Qleaf(3+1,5+1)       
      real incpar(3,4)   
      real laic
      real kbl
      real kdrt     
!     real fgl    
!     real fgrsh
!     real fgrsun
      real fslla     
      real refh
      real refs
      real scv
      real visabs
      real visabst
      real visabsth
      real visll
      real visl
      real visd
      real visdf
      real vispp
      real visshd
      real vissun
      real vist
      real phot_layer(3) !Total photosynthesis per canopy layer
      real DEC, SNDN, SNUP, CLOUDS, ISINB, S0N, lat_sim ! DSSAT's SOLAR/HMET
      real AZZON, BETA, RADHR, PARHR, AMTRH, FRDIFP, FRDIFR 
      real pi,rad
      
      !--- Gaussian arrangement for three points
      real :: gausr     = 0.3872983d0
!     real :: gaussx(3) = (/0.1127017,       0.5, 0.8872983/)
      real :: gaussw(3) = (/0.2777778, 0.4444444, 0.2777778/) 
      
      !--- Gaussian arrangement for five points
      real :: gaussx_5(5) = (/0.0469101, 0.2307534,       0.5, 0.7692465, 0.9530899/)
      real :: gaussw_5(5) = (/0.1184635, 0.2393144, 0.2844444, 0.2393144, 0.1184635/) 
      
      !--- Daily Expected PAR Ratio [PAR/SRAD]
!     real :: expected_par_ratio = 0.5d0
      
      !--- deg2rad
      parameter (pi=3.14159, rad=PI/180.)
      
      
      !--- Initialization
      dtga          = 0.d0
      photth        = 0.d0
      incpar        = 0.d0
      Acanopy       = 0.d0
      Qleaf         = 0.d0
      par_check     = 0.d0
      visabsth      = 0.d0

      Acanopy(1, 1) = amax

      !--- three point gaussian integration over day
      if (amax .lt. 1.d-10) return

      do ghour = 1, 3

          !--- Hour at Gaussian step
          hour = 12.d0 + dayl * 0.5 * (0.5d0 + (ghour - 2) * gausr)

          !--- Compute diffuse and direct irradiance at the specified hour
          !call radiat (daynr,hour,dayl,sinld,cosld,srad,sinb,pardir,pardif)
          
          !--- Get solar declination at hour
          call HANG(DEC, hour, lat_sim, AZZON, BETA)
          
          !--- Get global radiation and PAR at hour
          call HRAD(BETA, hour, ISINB, SNDN, SNUP, srad, RADHR)     
          call HPAR(hour, par, RADHR, SNDN, SNUP, srad,  PARHR) 
          
          !--- Get fraction of diffuse radiation
          call FRACD(BETA, CLOUDS, hour, RADHR, S0N, SNDN, SNUP, AMTRH, FRDIFP, FRDIFR)  

          !--- Get PAR fractions in [W m-2]
          pardir = PARHR * (1.d0 - FRDIFP) / 4.6 ! 4.6 is used to convert umol to J/s (1 J/s = W)
          pardif = PARHR * (FRDIFP)        / 4.6 ! 4.6 is used to convert umol to J/s (1 J/s = W)
          
          !--- Get sinb from HANG()
          sinb = SIN(BETA * rad)

          !--- Store incoming direct, difuse and total par radiation [W m-2]
          incpar(ghour,1) = hour
          incpar(ghour,2) = pardir
          incpar(ghour,3) = pardif
          incpar(ghour,4) = pardif + pardir

          !--- Extinction coefficients kdif,kdirbl,kdirt following Goudriaan (2016)
          refh  = (1.d0 - sqrt(1.d0 - scv))/(1.d0 + sqrt(1.d0 - scv))
          refs  = refh * 2.d0 / (1.d0 + 2.d0 * sinb)
          kbl   = 0.5d0 / sinb         ! Spherical model
          kdrt  = kbl * sqrt(1.0d0-scv)

          !--- Five point gaussian integration over lai at the given hour
          phot      = 0.d0
          visabs    = 0.d0
          do glai = 1, 5

              !--- LAI at canopy layer
              laic = lai * gaussx_5(glai)

              !--- Upper and lower lai layer aproximation
              if(glai .eq. 1)then
                  lai_sl_upper(glai)  = 0.d0
                  lai_sl_bottom(glai) = ((lai * gaussx_5(glai + 1)) + laic) / 2.d0
                  dlaic   = laic
              elseif(glai .eq. 5)then
                  lai_sl_upper(glai)  = ((lai * gaussx_5(glai - 1)) + laic) / 2.d0
                  lai_sl_bottom(glai) = lai
                  dlaic   = laic - (lai * gaussx_5(glai - 1))
              else
                  lai_sl_upper(glai)  = ((lai * gaussx_5(glai-1)) + laic) / 2.d0
                  lai_sl_bottom(glai) = ((lai * gaussx_5(glai+1)) + laic) / 2.d0
                  dlaic   = laic - (lai * gaussx_5(glai-1))
              endif

              !--- Canopy Layer LAI
              lai_sl(glai) = lai_sl_bottom(glai) - lai_sl_upper(glai)

              !--- Absorbed diffuse radiation (vidf), light from direct origine (vist) and direct light(visd)
              visdf   = (1.0d0-refh) * pardif * kdif  * exp(-kdif   * laic)
              vist    = (1.0d0-refs) * pardir * kdrt  * exp(-kdrt   * laic)
              visd    = pardir * kbl * exp(-kbl * laic)

              !--- Absorbed flux for shaded leaves [W/m2]
              visshd = visdf + vist - visd

              !--- Instantaneous Assimilation Rate of shaded leaves [kg/ha/h]
              phoshd = amax * (1.d0 - exp(-visshd * eff / amax))

              !--- Direct light absorbed by leaves perpendicular on direct beam and assimilation of sunlit leaf area
              !--- three point gaussian integration over lai layer (Added, following Goudriaan (2016))
              vispp   = pardir * (1. - scv) / sinb
              phosun  = 0.d0
              visll   = 0.d0
              do gl = 1, 5
                  vissun  = visshd + vispp * gaussx_5(gl)               ! [W/m2]
                  phos    = amax * (1.d0 - exp(-vissun * eff / amax))   ! [kg/ha/h/dgauss]
                  phosun  = phosun + phos   * gaussw_5(gl)              ! [kg/ha/h]
                  visll   = visll  + vissun * gaussw_5(gl)              ! [W/m2]
              enddo

              !--- fraction of sunlit leaf area (fslla)
              fslla   = exp(-kbl * laic)

              !--- Instantaneous assimilation rate of layer [kg/ha/h]
              photl   = fslla *   phosun  +     (1.d0 - fslla) * phoshd

              !--- Light intercepted by layer [W/m2]
              visl    = fslla *   visll   +     (1.d0 - fslla) * visshd  !Added, following Goudriaan (2016)              
              
              !--- Absorbed light (direct + difuse) at lai depth (glai) and time (ghour) [PPFD - micromol m-2 s-1]
              Qleaf(1,glai+1)         = laic
              Qleaf(ghour+1, 1)       = hour
              Qleaf(ghour+1, glai+1)  = visl * 4.6

              !--- Assimilation Rate at lai depth (glai) and time (ghour)  [micromol(CO2) m-2 s-1]
              Acanopy(1,glai+1)       = laic
              Acanopy(ghour+1, 1)     = hour
              Acanopy(ghour+1,glai+1) = photl * (1.e3/1.e4) / 3600.d0 / 44.d0 * 1.e6

              !--- integration over lai
              phot    = phot   + gaussw_5(glai) * photl
              visabs  = visabs + gaussw_5(glai) * visl

          enddo

          phott     = phot * lai
          visabst   = visabs * lai

          !--- integration over time (hour)
          photth    = photth   + gaussw(ghour) * phott
          visabsth  = visabsth + gaussw(ghour) * visabst
          par_check = par_check + gaussw(ghour) * (pardif + pardir)
      enddo

      !--- Photosynthesis per canopy layer
      phot_layer = 0.d0
      do glai = 1,3
          do ghour = 1, 3
              phot_layer(glai) = phot_layer(glai) + ((Acanopy(ghour+1,glai+1) / (1000./10000.) * (3600.) * 44. / 1000000. ) * gaussw(ghour) * gaussw(glai))!
          enddo
      enddo

      phot_layer = phot_layer * lai * dayl !integrated in g CO2 d-1 layer-1

      !--- Check incomming PAR integration method
      par_check = par_check * dayl * 3600.d0
      dvisabs   = visabsth  * dayl * 3600.d0
      par_ratio = par_check / (srad * 1e6)          ! This should be around 0.5
      

      !--- Fraction of light absorbed by the canopy
      frac_li   = dvisabs / par_check

      !--- Total Daily Gross Photosynthesis [g]
      dtga      = photth    * dayl

      return
    end subroutine totass        
    