*     dit is een kopie van het lexparse gebied !!!!!!!!

*     Token info buffer. This file defines a common  buffer
*     this is meant to exchange token information with other
*     subroutines.

*     ISP    - Start position where parsing of word started
*     IP     - Position where parsing is taking place
*     TOKEN  - Token number
      INTEGER ISP, IP, TOKEN

      COMMON /LEXTOKINFO/ ISP, IP, TOKEN
