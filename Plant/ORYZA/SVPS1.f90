!----------------------------------------------------------------------*
! SUBROUTINE SVPS1 (Subroutine Vapour Pressure Saturated no. 1)        *
! Authors: Daniel van Kraalingen                                       *
! Date   : 3-Feb-1991, Version: 1.0                                    *
! Purpose: This subroutine calculates saturated vapour pressure and    *
!          slope of saturated vapour pressure. Parameters of the       *
!          formula were fitted on the Goff-Gratch formula used in the  *
!          Smithsonian Handbook of Meteorological Tables. The          *
!          saturated vapour following the Goff-Gratch formula is also  *
!          available as a subroutine. (Note that 1kPa = 10 mbar)       *
!                                                                      *
! FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)      *
! name   type meaning                                     units  class *
! ----   ---- -------                                     -----  ----- *
! TMA     R4  Temperature at which to calculate pressure     C      I  *
! VPS     R4  Saturated vapour pressure                     kPa     O  *
! VPSL    R4  Slope of VPS at TMA                          kPa/C    O  *
!                                                                      *
! Fatal error checks: none                                             *
! Warnings          : TMA < -20, TMA > 50                              *
! Subprograms called: none                                             *
! File usage        : none                                             *
!----------------------------------------------------------------------*

      SUBROUTINE SVPS1 (TMA,VPS,VPSL)
      IMPLICIT NONE
      REAL TMA,VPS,VPSL
      SAVE

      IF (TMA.LT.-20..OR.TMA.GT.50.) WRITE (*,'(A,G12.5,A)') &
         ' WARNING from SVPS1: extreme temperature:',TMA,' d. Celsius'

      VPS  = 0.1*6.10588*EXP (17.32491*TMA/(TMA+238.102))
      VPSL = 238.102*17.32491*VPS/(TMA+238.102)**2

      RETURN
      END
