C======================================================================
C  TF_NFACTO, Subroutine
C
C  Determines N deficit in Wheat
C----------------------------------------------------------------------
C  Revision history
C  xx/xx/19xx     Written
C  02/08/1993 PWW Header revision and minor changes
C  03/29/2001 WDB Converted to modular form
C  12/01/2001 WDB Further modular conversion
C  02/25/2005 CHP Check for NFAC < 0.
C  06/27/2011 FSR created WH_NFACTO.for for APSIM NWheat adaptation
C JZW question, the codes are from DSSAT and only used for WH_OPHARV?
!  01/11/2018 KEP converted WH_ sub-routines to TF_.
C----------------------------------------------------------------------
C
C  Called by: TF_GROSUB
C
C  Calls  : None
C----------------------------------------------------------------------

      SUBROUTINE TF_NFACTO(DYNAMIC,               !Control
     %    TANC,TCNP,TMNC,                         !Inputs
     %    AGEFAC,NDEF3,NFAC,NSTRES)               !Outputs

      USE ModuleDefs
      USE TF_module
      IMPLICIT NONE
      SAVE
C----------------------------------------------------------------------
C                     Variable Declaration
C----------------------------------------------------------------------
      REAL        AGEFAC
      REAL        NDEF3
      REAL        NFAC
      REAL        NSTRES
      REAL        TANC
      REAL        TCNP
      REAL        TMNC
      INTEGER     DYNAMIC
C     ----------------------------------------------------------------


      IF(DYNAMIC.EQ.RUNINIT) THEN
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3 = 1.0
          NFAC = 1.0

      ELSEIF(DYNAMIC.EQ.SEASINIT) THEN
          NSTRES = 1.0
          AGEFAC = 1.0
          NDEF3 = 1.0
          NFAC = 1.0

      ELSEIF(DYNAMIC.EQ.INTEGR) THEN


! If the actual nitrogen content in the above ground biomass (TANC)
! decreases below a critical level (TCNP), then compute N stress
! based on proportion nitrogen below critical level (TCNP-TANC) and
! total nitrogen between the critical level and minimum
! level (TCNP-TMNC).

          NFAC   = 1.0 - (TCNP-TANC)/(TCNP-TMNC)
!          NFAC   = AMIN1 (NFAC,1.0)

!         2/25/2005 chp
!          NFAC   = AMAX1 (NFAC,0.001)

!Make nitrogen stress factor less sensitive during ISTAGE 3 and 4.
!JIL: This is the same as moving TCNP down during XSTAGE 1.5~5.5 ??
!     which will move TCNP closer to the original Jones (1983) function
!Note however that when NFAC=1, this transformation makes it 0.94,
!indicating that there will always be some N stress during this period.
! 10/02.2007 JIL Fixing this problem
!          IF (ISTAGE .EQ. 3 .OR. ISTAGE .EQ. 4) THEN          !
!           IF (NFAC .LE. 0.93) THEN                           !
!              NFAC = 1.0 - 1.80*EXP(-3.5*NFAC)                !
!           ENDIF                                              !
!!              NFAC = 1.082 - 1.82*EXP(-3.1*NFAC)             !
!          ENDIF                                               !

          NFAC   = AMIN1 (NFAC,1.0)
          NFAC   = AMAX1 (NFAC,0.001)

          !Compute nitrogen stress factor for reducing leaf expansion
          AGEFAC = 1.0
          AGEFAC = NFAC
          AGEFAC = AMIN1 (AGEFAC,1.0)

          !Make grain per plant calculation less sensitive to N stress
          NDEF3  = 1.0
          IF (NFAC .LT. 0.8) THEN
              NDEF3 = 0.2 + NFAC
          ENDIF
          NDEF3  = AMIN1 (NDEF3, 1.0)


          !Compute nitrogen stress factor for screen output only?
          !Note that NSTRES is not used to modify any processes
          NSTRES = 1.0
          NSTRES = NFAC*1.2 + 0.2
          IF (NFAC .GT. 0.5) THEN
              NSTRES = NFAC*0.4 + 0.6
          ENDIF
      ENDIF


      RETURN

      END SUBROUTINE TF_NFACTO


C----------------------------------------------------------------------
C                         DEFINITIONS
C----------------------------------------------------------------------
! AGEFAC      !Nitrogen stress factor affecting expansion (0-1)
! ISTAGE      !Plant growth stage
! NDEF3       !Nitrogen stress factor affecting grains per plant (0-1)
! NFAC        !Nitrogen stress factor based on actual and critical
!              nitrogen content in vegetative tissue
! NSTRES      !Nitrogen stress factor affecting growth (0-1)
! TANC        !Nitrogen content in above ground biomass, decimal
! TCNP        !Critical nitrogen concentration in tops, g N/g dry wt.
! TMNC        !Plant top minimum N concentration g N/g dry matter
! DYNAMIC     !Modular control variable
! RUNINIT     !DYNAMIC control variable
! SEASINIT    !DYNAMIC control variable
! RATE        !DYNAMIC control variable
! INTEGR      !DYNAMIC control variable
! OUTPUT      !DYNAMIC control variable
! SEASEND     !DYNAMIC control variable
C----------------------------------------------------------------------

!======================================================================
      !*! subroutine nwheats_set_nconc (pcnc, pmnc) ! replaced by JZW
      subroutine ntefs_set_nconc (xstag_nw, istage,            !Input
     &      zstage, VSEN,                                        !Input
     &      cnc, mnc)                                           !Output
!======================================================================
      USE TF_module
      implicit none
!      include    'nwheats.inc'          ! CERES_Wheat Common Block
!      include 'data.pub'
!      include 'error.pub'

*+  Sub-Program Arguments **OK**
      real cnc(mxpart)  !(OUTPUT) critical N concentration (g N/g part)
      real mnc(mxpart)  !(OUTPUT) minimum N concentration (g N/g part)
!      *! real      pcnc (*)               ! (OUTPUT) critical N concentration
                                       !   (g N/g part)
!      *!  real      pmnc (*)               ! (OUTPUT) minimum N concentration
                                       !   (g N/g part)
      REAL xstag_nw, zstage, VSEN ! add by JZW
      INTEGER ISTAGE ! add by JZW
*+  Purpose
*       calculate the critical N concentration below which plant growth
*       is affected.  Also minimum N concentration below which it is not
*       allowed to fall.  These are analogous to the water concentrations
*       of dul and ll.


*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ntefs_set_nconc')
*
!      real       rcnpc               ! root critical n percent (0-100)
!      parameter (rcnpc = 1.06)
*
      real       rmnpc                 ! root minimum N percent (0-100)
      parameter (rmnpc = 0.45)
*
cjh      real       tmncpc      ! minimum n concentration percent (0-1)
cjh      parameter (tmncpc = 0.45)

*- Implementation Section ----------------------------------

      !*! call push_routine (myname) ! comment out by JZW

      cnc = 0.0 ! replaced by JZW
      mnc = 0.0 ! replaced by JZW
      if (xstag_nw.gt.0.0) then
         cnc(root_part) = (2.10 - 0.14 * sqrt(zstage))/100.
         If (istage .eq. grnfil) then
            mnc(root_part) = 0.75 * cnc(root_part)
         else
            !*! pmnc(root) = rmnpc/100.0
            mnc(root_part) = rmnpc/100.0
         endif

!              the tops critical N percentage concentration is the stover
!              (non-grain shoot) concentration below which N concentration
!              begins to affect plant growth.

      !*! if (p1v .ge. 0.03)then
         if (VSEN .ge. 0.03)then
            cnc(stem_part) = - 5.01124 - 6.35067 * zstage
     :                  + 14.9578 * sqrt(zstage) + 0.223819*zstage**2
         else
            cnc(stem_part) =   7.45318 - 1.79078 * zstage
     :                  + 0.60928 * sqrt(zstage) + 0.0933967*zstage**2
         endif

         if (zstage .gt. 6.0) then
            cnc(stem_part) = cnc(stem_part) - (zstage - 6.0) * 0.140
         else
         endif

cnh note that cnc's can be negetive if istage>mature
         cnc(stem_part) = cnc(stem_part)/100.
         cnc(leaf_part) = cnc(stem_part)
         cnc(lfsheath_part) = cnc(stem_part)

             ! the  minimum N concentration is the N concentration
             ! below which N does not fall.

         mnc(stem_part) = (2.97 - 0.455*xstag_nw)/100.
cbak lower boundary for stover n % = 0.25%
         mnc(stem_part) = max(mnc(stem_part), 0.0025)
         mnc(leaf_part) = mnc(stem_part)
         mnc(lfsheath_part) = mnc(stem_part)
      else
       continue
      endif

      return
      end
!======================================================================
      subroutine ntefs_set_nfact (xstag_nw, istage,            !Input
     &      cnc, mnc, pl_nit, plantwt,                         !Input
     &      nfact)                                             !Output
!======================================================================
! 2023-01-26 chp removed unused variables in argument list: zstage
      USE ModuleDefs
      USE TF_module
      implicit none
      external warning, error

   !*!   include 'const.inc'              ! err_internal
   !*!   include    'nwheats.inc'          ! CERES_Wheat Common Block
   !*!   include 'data.pub'

*+  Purpose
*         Uses shoot nitrogen concentrations to calculate 0-1 N availability
*         factors.  Since all plant processes are not equally susceptible
*         to N stress, N deficiency factors are calculated from a 0-1 N
*         factor to affect different processes.
*           0 affects grain N potential
*           1 affects photosynthesis
*           2 affects leaf senescence, grain N concentration & cell expansion
*           3 affects grain number
*
*           nfac range is 0.001 to 0.95 or 1.0 for optimum conditions.
*           ndef - 1 range is 0.2012 to .98 or 1 for optimum conditions.
*           ndef - 2 range is .00095 to .9025 or .95 for optimum conditions.
*           ndef - 3 range is .201 to 1 for optimum conditions.
*
*         ???? check that returns 1 & 0 for optimum and zero conditions.

*+  Changes
*       020392 jngh specified and programmed
*       150692 jngh changed cnp to cnc

*+  Constant Values
      character  myname*(*)            ! name of subroutine
      parameter (myname = 'ntefs_set_nfact')

*+  Local Variables
      real  nfac    ! N factor type 0 (0-1)
      real  tanc    ! tops (stover) actual N concentration   (0-1)
      real  tcnc    ! tops (stover) critical N concentration (0-1)
      real  tmnc    ! tops (stover) minimum N concentration  (0-1)
      real  xnfac   ! available N as fraction of N capacity  (0-1)
      REAL xstag_nw  !, zstage
      REAL plantwt(mxpart) !plant part weights (g/plant)
      REAL cnc(mxpart) ! critical N concentration by plant part
      REAL mnc(mxpart) ! minimum  N concentration by plant part
      INTEGER istage
      REAL pl_nit(mxpart) !plant part nitrogen (g/plant)
      REAL nfact(10)
      character*78 msg(1)

*- Implementation Section ----------------------------------

          ! calculate 0-1 nitrogen factor

!*!      tanc = divide ((pl_nit(leaf) + pl_nit(stem) + pl_nit(lfsheath)),
!*!     :               (pl_wt(leaf) + pl_wt(stem) + pl_wt(lfsheath)), 0.0)
!*!      tcnc = divide (cnc(leaf)*pl_wt(leaf) + cnc(stem)*pl_wt(stem) +
!*!     :               cnc(lfsheath)*pl_wt(lfsheath),
!*!     :               (pl_wt(leaf) + pl_wt(stem) + pl_wt(lfsheath)), 0.0)
!*!      tmnc = divide (mnc(leaf)*pl_wt(leaf) + mnc(stem)*pl_wt(stem) +
!*!     :               mnc(lfsheath)*pl_wt(lfsheath),
!*!     :               (pl_wt(leaf) + pl_wt(stem) + pl_wt(lfsheath)), 0.0)
! replaced above by JZW
      if (plantwt(stem_part) .eq. 0.) then
          write(msg(1),*) "plant top wt is zero"
          call warning(1,"NWheat",msg)
          call error("NWheat",99," ",0)
      endif
      tanc =(pl_nit(leaf_part)+pl_nit(stem_part)+pl_nit(lfsheath_part))/
     &      (plantwt(leaf_part) + plantwt(stem_part) +
     &       plantwt(lfsheath_part))
      tcnc = (cnc(leaf_part) * plantwt(leaf_part) + cnc(stem_part) *
     &       plantwt(stem_part) + cnc(lfsheath_part) *
     &       plantwt(lfsheath_part))/
     &       (plantwt(leaf_part) + plantwt(stem_part) +
     &       plantwt(lfsheath_part))
      tmnc = (mnc(leaf_part) * plantwt(leaf_part) + mnc(stem_part) *
     &       plantwt(stem_part) + mnc(lfsheath_part) *
     &       plantwt(lfsheath_part))/
     &       (plantwt(leaf_part) + plantwt(stem_part) +
     &       plantwt(lfsheath_part))

      if (xstag_nw .gt. 1.1) then
         xnfac = (tanc - tmnc) / (tcnc - tmnc)
         !xnfac = max(xnfac, 0.02)

!        JZW changed on July 21, 2014 based on the request of Senthold
         xnfac = max(xnfac, 0.0) 

      else
         xnfac = 1.0
      endif
      !*! nfac = bound (xnfac, 0.0, 1.0) !replaced by JZW
      nfac = max (xnfac, 0.0)
      nfac = min(nfac, 1.0)
      if (istage.eq.germ) then
!           JZW:  IF (ISTAGE .GT. 0 .AND. ISTAGE .LE. 6) THEN   call WH_GROSUB if DYNAMIC = INTEGRATE, thus never go here
        nfact(1) = 1.0
        nfact(2) = 1.0
        nfact(3) = 1.0
        nfact(4) = 1.0
       !else if ((istage.ge.emerg).and.(istage.lt.mature)) then
      else if ((istage.ge.emergence).and.(istage.lt.mature)) then

         nfact(1) = 1.5 * nfac
        !*! nfact(1) = bound (nfact(1), 0.0, 1.0) replaced by JZW
        nfact(1) = max (nfact(1), 0.0)
        nfact(1) = min(nfact(1), 1.0)
cbak
         !      nfact(1) = 0.10 + 2. * nfac
         !      nfact(1) = bound (nfact(1), 0.0, 1.0)

         nfact(2) = nfac
         nfact(2) = max (nfact(2), 0.0)
         nfact(2) = min(nfact(2), 1.0)
         nfact(3) = nfac * nfac
         nfact(3) = max (nfact(3), 0.0)
         nfact(3) = min(nfact(3), 1.0)

         nfact(4) = xnfac**2
         nfact(4) = max (nfact(4), 0.0)
         nfact(4) = min(nfact(4), 1.0)

      endif
      return
      end

