*     status common /RDSTA/
*     ------------------------------------------------------
*     TOSCR  - flag enabling error message output to screen
*     TOLOG  - flag enabling error message output to logfile
*     IULOG  - unit number of logfile (when TOLOG is set)
*              if non-existent, RDINDX.LOG is created


      INTEGER IULOG
      LOGICAL TOSCR, TOLOG

      COMMON /RDSTA/ TOSCR , TOLOG, IULOG
