      SUBROUTINE OBSINI

!     initialises the observation/forcing system

      IMPLICIT NONE
      INTEGER       ITASK
      CHARACTER (1) CDUM1, CDUM2
      LOGICAL       LDUM1,LDUM2,LDUM3,LDUM4
      REAL          RDUM1
      SAVE

      ITASK = 1
      CDUM1 = ' '
      CDUM2 = ' '
      LDUM1 = .FALSE.
      LDUM2 = .FALSE.
      LDUM3 = .FALSE.
      LDUM4 = .FALSE.
      RDUM1 = -99.
      CALL OBSSYS (ITASK,CDUM1,CDUM2, &
                   LDUM1,LDUM2,LDUM3,LDUM4,RDUM1)

      RETURN
      END
