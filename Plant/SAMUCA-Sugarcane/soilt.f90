    
    subroutine soilt_sam(ys, phis, ratio_G, ratio_T,dlayer_cm,sw)
    
    implicit none
    !include 'Constants.fi'
    
!  Sub-Program Arguments
      real ys(200+1)                    ! (Output) magnitude of soil thermal admittance (W/m2/K)
      real phis(200+1)                  ! (Output) phase angle of soil thermal admittance (rad)
      real ratio_G(200+1)               ! ratio of heat flux between top and bottom layer boundaries ()
      real ratio_T(200+1)               ! ratio of temperature between top and bottom layer boundaries ()
      real dlayer_cm(200)               ! thickness of soil layer I (cm) -slthickness()
      real sw (200)                     ! soil water content of layer L (m3/m3) - swc()
      
!  Local Variables
      real Zdc           ! layer depth (decimetre)
      real VWC           ! volumetric soil water content of layer (m3/m3)
      real Cc
      real K
      double precision fun_A     
      double precision fun_M     
      real ll
      real topsw   ! soil water depth in top profile (cm)
      real subsw   ! soil water depth in sub-profile (cm)
      real zd      ! depth (cm)
	  complex YS1
      complex Y
      complex RG
      complex RT
      complex SRG(100)
      complex SRT(100)
      complex CN
      integer nj   ! layer counter
      integer i    ! layer counter
      integer j    ! layer counter
      integer nj7
      integer node
      real cumr

!  Constant Values
      real Pp         ! period length (s)
      parameter (Pp = 86400.0D0)

      real vsw(7)     ! volumetric soil water content of SoilT layers (m3/m3)
      real tlayr(7)   ! SoilT layer depths (7 is top, 1 is bottom)

!      DATA Pp / 86400. /
!      data tlayr / 0.,0.,1.,1.,1.,1.,1. /
!      data vsw / 7*0.0 /

      fun_M (CN) = cabs (CN)            !??
      fun_A (CN) = aimag (clog (CN))    !??

! --- Implementation Section ----------------------------------

      tlayr(1:2)    = 0.0
      tlayr(3:7)    = 1.0
      vsw(:)        = 0.0

      ! SoilT layers number 1 to 7 from bottom to top (reverse of dlayer).
      if (dlayer_cm(1) .ge. 6.) then
            ! we have a thick top layer. Split top layer into 6 layers
            ! and lump the other layers into one - a total of 7 layers.
            ! Top 5 thin layers
          
         topsw  = 0.
         zd     = 6.
         
        do 20 i = 3, 7
          zd = zd - 1.
          vsw(i) = .0125 - 0.25*sw(1) + 0.25*(sw(1) - .01)*zd !Warning: not explained relation
          topsw = topsw + vsw(i)
          
20      continue
        ! 6th layer is remainder of top dlayer.
        
        tlayr(2) = dlayer_cm(1) - 5.
        vsw(2) = (sw(1)*dlayer_cm(1) - topsw) / tlayr(2)
        
        ! now put the remaining layers (dlayer(2) onwards to 75 cm [?]) into one bottom layer !considering only up 75 cm soil profile
         tlayr(1) = 0.
         subsw = 0.
         
        do 30 j = 2, 10
          tlayr(1) = tlayr(1) + dlayer_cm(j)
          subsw = subsw + sw(j)*dlayer_cm(j)

          if (tlayr(1) .ge. 75.) go to 40
          
   30   continue

   40   vsw(1) = subsw / tlayr(1)

      else
        ! top layer is thin enough.
          
        do 50 j = 1, 4
          tlayr(j) = dlayer_cm(4)
   50     vsw(j) = sw(4)
        vsw(5) = sw(3)
        vsw(6) = sw(2)
        vsw(7) = sw(1)

        tlayr(5) = dlayer_cm(3)
        tlayr(6) = dlayer_cm(2)
        tlayr(7) = dlayer_cm(1)
      endif

      YS1 = (10., .7845) !? complex form is archaic
      NJ = 0
      NJ7 = 7
      cumr = 1.0
      
10    CONTINUE
    ! start at bottom layer (1) and loop up to the top layer (7)
      
      node = nj7 - nj
      NJ = NJ + 1
      zdc = tlayr(nj) / 100.
      vwc = vsw(nj)
      
      CALL GETCK (VWC, Cc, K)
      CALL ADMIT1 (Pp, Zdc, Cc, K, YS1, Y, RG, RT)
      
      YS1 = Y
      
    ! store results into real variables
      ys(node) = fun_m (y)
      
      write(*,*) fun_m(y)
      phis(node) = fun_a (y)
      ratio_G(node) = fun_m (RG)
      ratio_T(node) = fun_m (RT)
      cumr = 1.0/ratio_T(node) * cumr

      if (nj .lt. NJ7) GO TO 10!      
      
      return      
      
    end subroutine soilt_sam
    