*     VINT   - Value in case token is integer value
*     VFLOAT - Value in case token is floating point value
*     VTIME  - Value in case token is date/time value
*     VLOGIC - Value in case token is logical value
*     VTYPE  - Value type of TOKEN, 'F' or 'I' or 'L' or 'T' or 'C'

      INTEGER          VINT
      DOUBLE PRECISION VFLOAT, VTIME
      LOGICAL          VLOGIC
      CHARACTER        VTYPE*1

      COMMON /DECODE1/ VFLOAT, VTIME, VLOGIC, VINT
      COMMON /DECODE2/ VTYPE
