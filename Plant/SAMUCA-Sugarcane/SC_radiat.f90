subroutine radiat(daynr,hour,dayl,sinld,cosld,avrad,sinb,pardir,pardif)
! ----------------------------------------------------------------------
! --- Authors: this routine Astro is based on Sastro (Daniel van Kraalingen)
! --- calculates the fluxes of diffuse and direct photosynthetically
! --- active radiation from the total daily shortwave radiation actually
! --- received (avrad) for a given day of the year and hour of the day.
! --- the input variables dayl, sinld and cosld are calculated in astro.
! --- for more information: see spitters et al. (1988).
! ----------------------------------------------------------------------
      implicit none

      integer daynr

      real  aob,atmtr,avrad,cosld,dayl,dsinb,dsinbe,dso,frdif,hour
      real  par,pardif,pardir,pi,sc,sinb,sinld

      data    pi /3.1415926d0/
! ----------------------------------------------------------------------
! --- calculations on solar elevation
! --- sine of solar elevation sinb
      aob = sinld/cosld
      sinb = max (0.0d0,sinld+cosld*cos(2.0*pi*(hour+12.0d0)/24.0))
! --- integral of sinb
      dsinb = 3600.*(dayl*sinld+24.*cosld*sqrt(1.0d0-aob*aob)/pi)
! --- integral of sinb, corrected for lower atmospheric transmission
! --- at low solar elevations
      dsinbe = 3600.*(dayl*(sinld+0.4*(sinld*sinld+cosld*cosld*0.5))+12.0*cosld*(2.0d0+3.0*0.4*sinld)*sqrt(1.0d0-aob*aob)/pi)

! --- solar constant and daily extraterrestrial radiation
      sc = 1370.*(1.0d0+0.033*cos(2.0*pi*daynr/365.))
      dso = sc*dsinb

! --- diffuse light fraction from atmospheric transmission
      atmtr = avrad/dso
      if (atmtr.gt.0.75d0)  frdif = 0.23d0
      if (atmtr.le.0.75d0.and.atmtr.gt.0.35d0) frdif = 1.33d0-1.46*atmtr
      if (atmtr.le.0.35d0.and.atmtr.gt.0.07d0) frdif = 1.0d0-2.3*(atmtr-0.07d0)**2
      if (atmtr.le.0.07d0) frdif = 1.0d0

! --- photosynthetic active radiation, diffuse and direct
      par = 0.5*avrad*sinb*(1.0d0+0.4*sinb)/dsinbe
      pardif = min (par,sinb*frdif*atmtr*0.5*sc)
      pardir = par-pardif

      return

       
end subroutine radiat
    