*     Record info buffer. This file defines a common buffer
*     this is meant to exchange record information with other
*     subroutines.

*     limit MS-FORTRAN w.b. declaratie strings: 32k characters
*     limit VAX-FORTRAN 64k characters
*     limit ABSOFT-FORTRAN staat niet in de manual
*     limit LS-FORTRAN 32K characters

*     RECLEN - Declared length of buffer
*     STBLEN - Actual length of buffer
*     STBUF  - Buffer itself
*     STBUF2 - Buffer as single character array (equivalenced with STBUF)
*     ASCII  - Array with ASCII values of the buffer
*     RECNO  - Record number of input file

*     Note:    RECLEN-1 is the actual length on file
*              (rdlex uses 1 extra character), so if RECLEN=256, significant
*              record length on file is 255
      INTEGER RECLEN, STBLEN

*:-C= Significant record length on data file+1
      PARAMETER (RECLEN=1024)
      CHARACTER*(RECLEN) STBUF
      CHARACTER STBUF2(RECLEN)*1
      EQUIVALENCE (STBUF,STBUF2)
      INTEGER ASCII(RECLEN), RECNO

      COMMON /RDRECINFO1/ STBLEN, ASCII, RECNO
      COMMON /RDRECINFO2/ STBUF
