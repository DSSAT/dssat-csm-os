      SUBROUTINE RDERRI (xFILEIN,xIUDF,xIULOG,xTOSCR,xTOLOG)

      IMPLICIT NONE

*     Formal parameters
      CHARACTER*(*) xFILEIN
      INTEGER       xIUDF,xIULOG
      LOGICAL       xTOSCR, xTOLOG

*     Local variables
      INCLUDE 'rdfilinf.inc'
      INCLUDE 'rdstainf.inc'
      INCLUDE 'rderrinf.inc'

      SAVE

      FILEIN = xFILEIN
      ILFILE = LEN_TRIM (FILEIN)
      IUDF   = xIUDF
      IULOG  = xIULOG
      TOSCR  = xTOSCR
      TOLOG  = xTOLOG
      INERR   = 0
      INWAR   = 0

      RETURN
      END
