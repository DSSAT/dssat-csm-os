      REAL FUNCTION UNIFL()
      IMPLICIT NONE

*     High quality pseudo random number generator.

*     This pseudo random generator is fully based on FUNCTION UNIFL
*     in the second edition (1987) of Bratley et al. (see below).
*     A logical INIT has been added to the original program in order
*     to include seeds in the program (implicit initialization).

*     This generator is of the so called combined type. It does
*     not behave pathologically with the Box-Muller method for the
*     generation of normal variates, as do the commonly used linear
*     congruential generators (see also comments in FUNCTION BOXMUL).

*     The algorithm is:  X(i+1) = 40014 * X(i) mod (2147483563)
*                        Y(i+1) = 40692 * Y(i) mod (2147483399)
*                        Z(i+1) = (X(i+1)+Y(i+1)) mod (2147483563)
*     The random number returned is constructed dividing Z by its
*     range. The period of the generator is about 2.30584E+18.
*     The algorithm originates from L'Ecuyer (1986). In Bratley
*     et al.(page 332) more information can be found on seeds
*     and periods of X and Y.

*     References:
*     Bratley, P., B.L. Fox, L.E. Schrage. 1983. A guide to simulation
*         Springer-Verlag New York Inc. 397 pp.
*     L'Ecuyer,P. (1986). Efficient and portable combined pseudo-
*         random number generators. Commun. ACM.

*     FORMAL_PARAMETERS:
*     none

**    local variables
      INTEGER JX,K
      DIMENSION JX(3)
      LOGICAL INIT
      SAVE
      DATA INIT/.FALSE./

      IF (.NOT.INIT) THEN
*        initialize generator ; see for seeds Bratley (1983)
         JX(2) = 1122334455
         JX(3) = 1408222472
         INIT = .TRUE.
      END IF

*     get next term in first stream = 40014 * JX(2) mod 2147483563
      K = JX(2) / 53668
      JX(2) = 40014 * (JX(2) - K * 53668)  -  K * 12211
      IF (JX(2).LT.0)  JX(2) = JX(2) + 2147483563

*     get next term in the second stream = 40692 * JX(3) mod 2147483399
      K = JX(3) / 52774
      JX(3) = 40692 * (JX(3) - K * 52774)  -  K * 3791
      IF (JX(3).LT.0)  JX(3) = JX(3) + 2147483399

*     set JX(1) = ((JX(3) + 2147483562 - JX(2)) mod 2147483562) + 1
      K = JX(3) - JX(2)
      IF (K.LE.0)  K = K + 2147483562
      JX(1) = K

*     put it on the interval (0,1)
      UNIFL = K * 4.656613E-10

      RETURN
      END
