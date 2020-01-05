    
    subroutine admit1(P, Zdc, C, K, YS1, Y, RG, RT)
    
    implicit none
    !include 'Constants.fi'
    
!  Sub-Program Arguments
      real P         ! (Input) period length (s)
      real Zdc       ! (Input) layer depth (decimetre)
      real C         ! (Input)
      real K         ! (Input)
      COMPLEX YS1    ! (Input)
      COMPLEX Y      !(Output)
      COMPLEX RG     !(Output)
      COMPLEX RT     !(Output)

!  Local Variables
      real W
      real D
      COMPLEX YINF
      COMPLEX R
      COMPLEX SINH
      COMPLEX COSH
      COMPLEX TANH
      
      real :: pi = 3.14159265

! Implementation Section ----------------------------------


      W = 2.*PI / P
      D = SQRT(2.*K / (W*C))
      YINF = (1.,1.)*SQRT(W*C*K / 2.)
      R = (1.,1.)*Zdc / D
      SINH = (CEXP(R) - CEXP( - R)) / 2.
      COSH = (CEXP(R) + CEXP( - R)) / 2.
      TANH = SINH / COSH

      RG = SINH*YINF / YS1 + COSH
      RT = SINH*YS1 / YINF + COSH
      Y = YINF*(TANH + YS1 / YINF) / (TANH*YS1 / YINF + 1.)
     
      return   
    
    end subroutine admit1