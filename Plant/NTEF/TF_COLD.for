C=======================================================================
C  TF_COLD, Subroutine, F.S. Royce.  
C  From APSIM nwheats (v 0.3) routines: vernalization, cold_hardening, 
C  frost_leaves, frost_tillers  and vfac 
C-----------------------------------------------------------------------
C  Calculates vernalization factor, cumulative vernalization, cold  
C  hardening, senesce leaf area and tillers due to frost
C-----------------------------------------------------------------------
C  REVISION HISTORY
C  09/11/2011     Written for WHAPS wheat model.
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
C-----------------------------------------------------------------------
C  Called by  : TF_PHENOL  (in nwheats.for, by nwheats_crppr only)
C  Calls      : None
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C The statements begining with !*! are refer to APSIM source codes 
C Call order from nwheats_crppr:
!*!   nwheats_vernalization 
!*!   nwheats_cold_hardening
!*!   nwheats_frost_leaves  
!*!   nwheats_frost_tillers
!*!   nwheats_phase 
C Call from nwheats_phase:
!*!      nwheats_ppfac  [unless needed for vernal/cold hard, leave in Phenol]
!*!      nwheats_pstag  [easier using division: sumstgdtt(stagno)/pgdd(stagno)]
!*!      nwheats_vfac
C-----------------------------------------------------------------------
C=======================================================================
      SUBROUTINE TF_COLD (CONTROL, ISWITCH, FILEIO,        !Input !IDETO
     &    istage, leafno, PLTPOP,  VREQ, tbase,            !Input
     &    tempcr, tiln, VSEN, weather,                     !Input
!    &   !YRDOY, YRPLT, TMAX, TMIN,                        !Input
     &    pl_la, plsc,  Tthrshld, frostf, crownT, SNOWky,  !In/Out
     &    nwheats_vfac, sen_la, vd, vd1, vd2)              !Output
C-----------------------------------------------------------------------
      USE ModuleDefs
      USE TF_module
      IMPLICIT NONE
      EXTERNAL SNOWFALL, WARNING
      SAVE
C----------------------------------------------------------------------

C                             Define Variables
C----------------------------------------------------------------------
      INTEGER     DYNAMIC         

C  FSR added coefficients from WHAPS cultivar file
      REAL        cumph_nw(11)
      REAL        cumvd
      REAL        frost_fr
      REAL        frost_temp
!     Nwheat using Tthrshld to replace frost_temp
      PARAMETER   (frost_temp = -6.0)  
      Real        Tthrshld, frostf, crownT
!     REAL        GRNO
      REAL        hi     ! Hardening Index ??  (deduced from context)
      REAL        hti    ! Total Hardening Index ??
      PARAMETER   (hti = 1.0)  ! from NWheat 
      REAL        maxsen
      REAL        nwheats_vfac
!     REAL        P5 
      REAL        pl_la
      REAL        PLTPOP   !plant density per m2
      REAL        plsc(20) !Plant leaf area array by phylochron interval
!     REAL        PPSEN
      REAL        VREQ
      REAL RAIN, WATAVL
!     REAL        RGFI
      REAL        sen_la  !Senesced leaf area for plant (NWheat)
      REAL        snow 
      integer     SNOWky
      REAL        tbase
      REAL        temkil
      REAL        tempcr
      REAL        tiln
      REAL        TMIN
      REAL        TMAX
      REAL        vd
      REAL        vd1
      REAL        vd2
      REAL        vfac
      REAL        VSEN
!     REAL        XLAT
!     INTEGER     DAP 
      INTEGER     istage
      INTEGER     leafno
!     INTEGER     YRDOY     
      CHARACTER*30 FILEIO  
!     CHARACTER*1  IDETO  
      CHARACTER*1  ISWWAT
      CHARACTER*78 MSG(10)
!     INTEGER MDATE, TIMDIF
!           ------------------------------------------------------------------
!     Define constructed variable types based on definitions in
!     ModuleDefs.for.
      TYPE (ControlType) CONTROL
      TYPE (SwitchType)  ISWITCH
!     Type (ResidueType) SENESCE
      TYPE (WeatherType) WEATHER   

!     Transfer values from constructed data types into local variables.
      DYNAMIC = CONTROL % DYNAMIC
      FILEIO  = CONTROL % FILEIO
      ISWWAT  = ISWITCH % ISWWAT
      TMAX    = WEATHER % TMAX
      TMIN    = WEATHER % TMIN
      RAIN    = WEATHER % RAIN
C-----------------------------------------------------------------------
C      Initialize local variables
!----------------------------------------------------------------------
!         DYNAMIC = RUNINIT OR DYNAMIC = SEASINIT
! ---------------------------------------------------------------------
      IF (DYNAMIC.EQ.RUNINIT .OR. DYNAMIC.EQ.SEASINIT) THEN
!       Do this just once in RUNINIT or SEASINIT
            cumvd        = 0.
            nwheats_vfac = 0.
            vd           = 0.
            vd1          = 0.
            vd2          = 0.
            vfac         = 0.
         IF (DYNAMIC.EQ.SEASINIT)  CALL SNOWFALL (SEASINIT,
     &    TMAX, RAIN,                                     !Input
     &    SNOW, WATAVL)                                   !Output
       ELSE    !  DYNAMIC = anything except RUNINIT OR SEASINIT? FSR
!*!      DAP   = MAX(0,TIMDIF(YRPLT,YRDOY))
C-----------------------------------------------------------------------
C Vervalization calculations 
C-----------------------------------------------------------------------
       if     (          cumvd .lt. VREQ  ! VREQ was 'reqvd' in Nwheat
     &                        .and.
     &       (istage .eq.emergence .or. istage .eq. germ)   )
     & then
 
         ! The cumulative vernalization has not reached the required
         ! level of vernalization 

 
         if (TMIN .lt. 15. .and. TMAX .gt. 0.0) then
            vd1 = 1.4 - 0.0778 * tempcr
            vd2 = 0.5 + 13.44 / (TMAX-TMIN + 3.)**2 * tempcr
            vd = MIN(vd1, vd2)
            vd = MAX(vd, 0.0)
            cumvd = cumvd + vd
 
         else
            ! too cold or too warm - no vernalization
         endif
 
         if (TMAX .gt. 30. .and. cumvd .lt. 10.) then
            ! high temperature will reduce vernalization
            cumvd = cumvd - 0.5*(TMAX - 30.)
            cumvd = MAX(cumvd, 0.0)
         else
         endif
 
      else
         ! vernalization is complete
      endif

C-----------------------------------------------------------------------
!  Cold hardening section
C-----------------------------------------------------------------------

      if (istage .lt. emergence .and. istage .gt. mature) then
!*! this statement currently does not make sense: istage < 1 AND > 6 
         ! There is no hardening in this growth stage - do nothing
 
cbak i cant see that these do anyhing. neil .... please check.
 
!      else if (hi .lt. hti .and. tempcr .lt. tbase - 1.) then
!         ! it is too early for cold hardening - do nothing
 
!      else if (hi .eq. 0.0 .and. tempmn .gt. tbase - 1.) then
!         ! not cold enough to initiate cold hardening - do nothing
 
      else
         if (hi .lt. hti) then
            ! early cold hardening
            ! --------------------
cbak tempcr is the average crown temperature. things have to be very cold for
cbak  it to get below tbase-1. maybe this was intended to be tempcn ??????
 
            if (tempcr .lt. tbase -1.) then
               hi = hi + 0.1 - (tempcr - (tbase + 3.5))**2/506
 
            else
             ! too warm for early hardening'
            endif
         else
         endif
 
cbak the second stage of cold hardening
 
         if (hi .ge. hti) then
            ! late cold hardening
            ! -------------------
            if (tempcr .le. tbase + 0.) then
               hi = hi + 0.083
!*!            hi = u_bound (hi, 2.*hti)
               hi = MIN(hi, 2.*hti)
            else
            endif
         else
         endif
 
cbak the "de-hardening process"
 
         if (TMAX .ge. tbase + 10.) then
            hi = hi + 0.2 - 0.02 * TMAX
cbak "dehardening" proceeds at twice the sspeed in stage 2 hardening
 
            if (hi.gt.hti) then
               hi = hi + 0.2 - 0.02 * TMAX
            else
            endif
!*!         hi = l_bound (hi, 0.0)
            hi = MAX(hi, 0.0)
         else
         endif
 
      endif
      
!      Prevent negative values in cold weather scenarios (TF - 01/18/2022)
      hi = MAX(hi,0.0) 
C-----------------------------------------------------------------------
! Senesce leaf area due to frost
C-----------------------------------------------------------------------
      if (istage .ge. emergence .and. istage .le. mature) then
!       JZW change July, 2015, replace frost_temp by Tthrshld in *.spe 
        !if (TMIN .lt. frost_temp) then !APSIM code
        if (TMIN .le. Tthrshld)  then 
 !            snow = 0.0 !APSIM code 
cbak this next function appears to have a bug in it !!!!!!
cbak temporarily replaced ..... i know the code is a mess !!!!!!!!!!
cbak   note hardening is inoperative
 
!            frost_fr = (0.020 * hi - 0.10) *
!     :         (tempmn * 0.85 + tempmx*.15 + 10.0 + 0.25 * snow)
          ! if (TMIN .le. -5.0)  then
          if (SNOWky .eq. 1) then ! turn on snow effect switch 
            ! 10 % of leaf area frosted for every degree less
            !frost_fr = (0.0-TMIN - 5) * 0.10  !JZW changed in Apr, 2014
              CALL SNOWFALL (RATE,
     &         TMAX, RAIN,                                 !Input
     &         SNOW, WATAVL)                               !Output
               frost_fr = (0.020 * hi - 0.10) *
     &                 (TMIN * 0.85 + TMAX *.15 + 10.0 + 0.25 * snow)  
          else ! turn off snow effect switch frostf will not be used??
               !  ! 10 % of leaf area frosted for every degree less
               !frost_fr = (0.0-TMIN - 5) * 0.10 !APSIM code
               frost_fr = (0.0-TMIN - 5) * frostf !JZW changed July 2015
          endif
 
!*!         frost_fr = bound (frost_fr, 0.0, 0.96)
            frost_fr = MAX(frost_fr, 0.0)
            frost_fr = MIN(frost_fr, 0.96)
            ! senesce leaf area due to frost - senescence cannot reduce
            ! green leaf area per tiller below 500 mm2.
 
            sen_la =  sen_la + frost_fr * (pl_la - sen_la)
 
cbak  maxsen can become -ve when pla <500
            maxsen = pl_la - 500. * tiln
 
cbak  add a lower boundary on leaf senesence due to frost
!*!         maxsen = l_bound (maxsen, 0.0)
            maxsen = MAX(maxsen, 0.0)
 
!*!         sen_la = u_bound (sen_la, maxsen)
            sen_la = MIN(sen_la, maxsen)
 
          if (maxsen .gt. 0.0) then
            write (MSG(1),*) 'We have frost damage on leaf area today!'
            write (MSG(2),*) 'Min temp = ', TMIN
            write (MSG(3),*) ' current sen_la =', sen_la
            write (MSG(4),*) ' current pla=', pl_la
            CALL WARNING(4,"NWheat",MSG)
          else
          ! insufficient leaf area per tiller for frosting
          endif
 
            do 100 leafno = 1, cumph_nw(istage)+2
               plsc (leafno) = plsc (leafno) * (1.0 - frost_fr)
  100       continue
 
        else
            ! not cold enough for frosting
               frost_fr = 0.
        endif
      else
         ! There is nothing to frost in this growth stage
      endif
 
C-----------------------------------------------------------------------
! Senesce tillers due to frost
C-----------------------------------------------------------------------

      if (istage .ge. emergence .and. istage .le. mature) then
!        JZW replace frost_temp by Tthrshld in *.spe July 2015
         !if (TMIN .le. frost_temp) then apsim code
         if (TMIN .le. Tthrshld)  then 
            ! it is cold enough to frost tillers depending on cold
            ! hardiness of the plant
!*!            call nwheats_crown_temp (tempcn, tempcx)
 
!*!         tempcr = (tempcn+tempcx)/2.0
            temkil = tbase - 6. - 6. * hi
 
         ! Kill tillers based on mean crown temperature
 
            if (temkil .gt. tempcr) then
               if (tiln .ge. 1.) then
!                   Prevent negative values in cold weather scenarios (TF - 01/18/2022)
                   if(tempcr - temkil .gt. 0.0) then
                      tiln = tiln * (0.9 - 0.02 * (tempcr - temkil)**2)
!                      tiln = tiln * (0.9 - crownT * (tempcr - temkil)**2)
                   else
                      tiln = 0
                   endif
      
                write (MSG(1),*) ' Killing tillers due to frost'
                CALL WARNING(1,"NWheat",MSG)
               else
               endif
 
               if (tiln .lt. 1.) then
                  write (MSG(1),*) 'Killing tillers due to frost'
                  CALL WARNING(1,"NWheat",MSG)
!                 PLTPOP = PLTPOP * (0.95 - 0.02 * (tempcr - temkil)**2)
                  PLTPOP = PLTPOP*(0.95 - crownT * (tempcr - temkil)**2)
!                 Fixed PLTPOP of reaching negative values in extreme cold weather (TF - 01/18/2022)
                  IF (PLTPOP .LT. 0.0) THEN 
                     PLTPOP = 0
                  ENDIF
                  tiln = 1.
               else
               endif
            else
            endif
         else
            ! not cold enough for frosting
         endif
      else
         ! There is nothing to frost in this growth stage
      endif

C-----------------------------------------------------------------------
C Low temperature yield warnings
C-----------------------------------------------------------------------

      if (istage .eq. endear .and. TMIN .lt. -1.0) then
! later write the the following three commented-out lines to appropriate output locations
        !*!  write (*,*) 'Risk of frost at flowering lowering yield'     
        !*!  write (*,*) ' Min. air temperature =', tempmn, 'oC'
      else
        !*!  No chance of frost
      endif

C-----------------------------------------------------------------------
C Calculation of vernalization factor  (last section)
C-----------------------------------------------------------------------

       if (istage .ge. emergence
     &     .and.
     &     istage .le. endjuv)
     &    then

         vfac = 1. - VSEN * (VREQ - cumvd)  ! VSEN replaces p1v in CUL
           nwheats_vfac = MIN(vfac, 1.0)
           nwheats_vfac = MAX(vfac, 0.0)
 
       else
         nwheats_vfac = 1.0
       endif
C-----------------------------------------------------------------------
      ENDIF
!     JZW strong: when vfacac=1.007694,nwheats_vfac=1, but when return nwheats_vfac=1.007694??  
      RETURN 
      END SUBROUTINE TF_COLD

C-----------------------------------------------------------------------
!     TF_COLD VARIABLES: 
C-----------------------------------------------------------------------
! DAP    Number of days after planting (d)
! IDETO  Switch for printing OVERVIEW.OUT file 
! MDATE  Harvest maturity (YYDDD)
! TMIN   Minimum daily temperature (°C)
! VSEN   CULTIVAR parameter: sensitivity to vernalisation  (scale 1-5; NWheat)
! WLFDOT Leaf weight losses due to freezing (g[leaf]/m2-d)
! YRDOY  Current day of simulation (YYDDD)
! YRPLT  Planting date (YYDDD)
!-----------------------------------------------------------------------
!     END TF_COLD SUBROUTINE
!-----------------------------------------------------------------------
