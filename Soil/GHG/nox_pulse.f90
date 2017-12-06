!=======================================================================
! DayCent nox_pulse, Subroutine
!=======================================================================
!
!/*              Copyright 1993 Colorado State University                    */
!/*                      All Rights Reserved                                 */
!
!/*****************************************************************************
!**
!**  FILE:      nox_pulse.c
!**
!**  FUNCTION:  float nox_pulse() 
!**
!**  PURPOSE:   
!**  
!**  INPUTS:
!**    ppt  - daily precip (cm)
!**    snow - snow cover (cm SWE)
!**
!**  GLOBAL VARIABLES:
!**    None
!**
!**  LOCAL VARIABLES:
!**    cumppt[] - circular array holding precipitation values
!**    ii       - loop control variable
!**    indx     - current array index
!**    mptr     - starting position in mtplr circular array 
!**    mtplr[]  - circular array ??
!**    nph      - starting position in ph circular array
!**    npl      - starting position in pl circular array
!**    npm      - starting position in pm circular array
!**    nppt     - starting position in cumppt circular array
!**    pflag    - ??
!**    ph[]     - circular array ??
!**    PHDAYS   - ?? (13)
!**    pl[]     - circular array ??
!**    PLDAYS   - ?? (2)
!**    pm[]     - circular array ??
!**    PMDAYS   - ?? (6)
!**    PPTDAYS  - number of consecutive days to track precipitation values (15)
!**    retval   - return value, increase of NO due to moisture and rain >= 1.0
!**    sumppt   - sum of precipitation values in cumppt array
!**
!**  OUTPUTS:
!**    retval - increase of NO due to moisture and rain >=1.0
!**
!**  CALLED BY:
!**    trace_gas_model()
!**
!**  CALLS:
!**    max3 - return the maximum of three input values
!**
!*****************************************************************************/
      Subroutine nox_pulse (dynamic, rain, snow, nox_puls)
      use ModuleDefs
      save

      integer, intent(in) :: dynamic
      real, intent(in) :: rain, snow
      real, intent(out) :: nox_puls

      integer, parameter ::   &
          PPTDAYS = 15,       &
          PLDAYS = 2,         &
          PMDAYS = 6,         &
          PHDAYS = 13 

      real cumppt(0:PPTDAYS-1)    !0:14
      real pl(0:PLDAYS-1)         !0:1         
      real pm(0:PMDAYS-1)         !0:5
      real ph(0:PHDAYS-1)         !0:12
      real mtplr(0:PHDAYS-1)      !0:12

      integer npl, npm, nph, nppt, mptr
      integer pflag

      real sumppt
      integer ii, indx

! =================================================================
      select case (dynamic)
! =================================================================
      case (runinit, seasinit)
! =================================================================

      cumppt = 0.0
      pl     = 1.0
      pm     = 1.0
      ph     = 1.0
      mtplr  = 1.0

      npl   = 0
      npm   = 0
      nph   = 0
      nppt  = 0
      mptr  = 0
      pflag = 0

      nox_puls = 1.0

!     write(334,'(" nppt   rain cumppt sumppt  npl     pl  npm     pm  nph     ph mptr  mtplr pflag nox_puls")')
!     write(334,'(i5,3f7.2,4(i5,f7.3),i6, f8.3)')  &
!        nppt, rain, cumppt(nppt), sumppt, npl, pl(npl), npm, pm(npm),   &
!        nph, ph(nph), mptr, mtplr(mptr), pflag, nox_puls

! =================================================================
      case (rate)
! =================================================================
      sumppt = 0.0
      cumppt(nppt) = rain
      do ii=1, PPTDAYS-1
        indx = Mod(nppt+ii,PPTDAYS)
        sumppt = sumppt + cumppt(indx)
      enddo

      if (snow > 0.0) then
        mtplr(mptr) = 0.0

      else if ((sumppt <= 1.0) .and. (rain > 0.1)) then
 !    /* initiate new pulse */
        if (rain < 0.5) then
          do ii=0, PLDAYS-1
            indx = MOD(npl+ii,PLDAYS)
            pl(indx) = 2.8 * exp(-0.805 * (ii+1))
          enddo
          pflag = 2

        else if ((rain >= 0.5) .and. (rain <= 1.5)) then
          do ii=0, PMDAYS-1
            indx = MOD(npm+ii,PMDAYS)
            pm(indx) = 3.67 * exp(-0.384 * (ii+1))
          enddo
          pflag = 6

        else
          do ii=0, PHDAYS-1
            indx = MOD(nph+ii,PHDAYS)
            ph(indx) = 4.615 * exp(-0.208 * (ii+1))
          enddo
          pflag = 13

        endif
        
        mtplr(mptr) = amax1(pl(npl), pm(npm), ph(nph))
        pflag = pflag - 1

      else if (pflag > 0) then
        mtplr(mptr) = amax1(pl(npl), pm(npm), ph(nph))
        pflag = pflag - 1

      else
        mtplr(mptr) = 1.0
      endif

      nox_puls = mtplr(mptr)

!     write(334,'(i5,3f7.2,4(i5,f7.3),i6,f8.3)')  &
!        nppt, rain, cumppt(nppt), sumppt, npl, pl(npl), npm, pm(npm),   &
!        nph, ph(nph), mptr, mtplr(mptr), pflag, nox_puls

      pl(npl) = 1.0
      pm(npm) = 1.0
      ph(nph) = 1.0
      
!     /* increment pointers in circular arrays */
      npl = MOD(npl+1, PLDAYS)
      npm = MOD(npm+1, PMDAYS)
      nph = MOD(nph+1, PHDAYS)
      nppt = MOD(nppt+1,PPTDAYS)
      mptr = MOD(mptr+1,PHDAYS)

! =================================================================
      end select
! =================================================================

      return
      end subroutine nox_pulse
!=======================================================================

