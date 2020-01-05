    
subroutine tridag(N,A,B,C,R,U,ierror)
!***********************************************************************
!* Adapted in: 18/07/2017 (Murilo Vianna)    
!* Purpose:    Solves for a vector U a tridiagonal linear set.         *
!* Uses Thomas Algorithim
!  References:                                                         *
!* Press, W.H., B.P. Flannery, S.A. Teukolsky & W.T. Vetterling, 1989. *
!* Numerical Recipes in FORTRAN. Cambridge University Press, New York. *
!* pp 40-41                                                            *
!***********************************************************************
!* Input:     N -      Number of equations                             *
!*            A,B,C -  Coefficients of the matrix                      *
!*            R -      known vector                                    *
!* Output:    U -      solved vector                                   *
!***********************************************************************
      implicit none
      !include 'Constants.fi'
      
!     Called by soiltemp => tridag (numnod, thoma, thomb, thomc, thomf, tsoil,ierror)
! --- global declarations
! --- (i.i) input
      integer   n, ierror
      real    a(n), b(n), c(n), r(n)
! --- output
      real    u(n)
! --- parameters
      real  small
      parameter (small = 0.3d-37)

! --- local declarations
      integer   i
      real    gammav(5000), beta
      character messag*200
! ----------------------------------------------------------------------

      ierror = 0

! --- (1) if b(1)=0 then rewrite the equations as a set of order n-1
!     to eliminate u(2)

      if (abs(b(1)).lt.small) then
        messag = 'During the numerical solution the factor b(1)'//' became too small !'
        ierror = 1000
!        call fatalerr ('tridag',messag)
      else
! ---   (2) decomposition and forward substitution
        beta = b(1)
        u(1) = r(1) / beta
        do i = 2, n
            gammav(i) = c(i-1) / beta
            beta = b(i) - a(i)*gammav(i)

            ! ---     (2.1) if beta=0 then go to another algorithm including
            ! ---     elimination with pivoting
            if (abs(beta).lt.small) then
                messag = 'during the numerical solution the factor beta'//' became too small !'
                ierror = 1000+i
                !            call fatalerr ('tridag',messag)
                messag = messag ! for Forcheck
            else
                u(i) = (r(i) - a(i)*u(i-1)) / beta
            end if
        enddo

! ---   (3) back substitution
        do i = n-1, 1, -1
          u(i) = u(i) - gammav(i+1)*u(i+1)
        end do
      end if

      return
end subroutine
    