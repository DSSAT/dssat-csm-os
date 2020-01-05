    
    subroutine getck(VWC, C, K)
    !Added to suport soilT calculations
    !Adapted from APSIM source code
    !Warning: not explained empirical relations (no documentation or coments for this routine)
        
    implicit none

!  Sub-Program Arguments
      real VWC      ! (Input)
      real C        ! (Output)
      real K        ! (Output)

!  Local Variables
      real x

!  Constant Values
      
      real C0
      parameter (C0 = 1.4D6)
      real C1
      parameter (C1 = 4.18D6)
      real K0
      parameter (K0 = 0.32D0)
      real K1
      parameter (K1 = 1.18D0)

! Implementation Section ----------------------------------

      C = C0 + C1*VWC
      x = 2.4*vwc
      if (vwc .gt. 0.075) x = x + 35.0*(vwc - 0.075)**2
      if (vwc .gt. 0.15)  x = x - 750.0*(vwc - 0.15)**3
      if (vwc .gt. 0.225) x = 1.0
      K = K0 + K1*x
      
      return    
    end subroutine getck
  