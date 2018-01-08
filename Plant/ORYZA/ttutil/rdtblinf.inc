*     table info common /RDTBL1/ and /RDTBL2/
*
*     TBLERR - flags the presence of an error
*     NAMERR - flags presence of error in name part
*     INAMES - number of names until first error
*     INLMX  - maximum length of variable names
*     NCOLMX - maximum number of columns
*     NCOL   - actual number of columns
*     ICOL   - column where processing takes place
*     NVAL   - number of values
*     TBLNAM - array with names
*     TBLPNT - array with pointers
*     TBLTYP - array with column datatypes
*     TBLLIN - array with line numbers for names of the table
*     TBARAY - = 'x' table contains array variable(s)

      INTEGER INAMES, NCOLMX, INLMX, NCOL, ICOL, NVAL, TBLPNT, TBLLIN
      CHARACTER TBLTYP*1, TBARAY*1, TBLNAM*31
      LOGICAL TBLERR, NAMERR

*:-C= Maximum number of columns in a table
      PARAMETER (NCOLMX=40)
      DIMENSION TBLNAM(NCOLMX), TBLPNT(NCOLMX)
      DIMENSION TBLTYP(NCOLMX), TBLLIN(NCOLMX)

      COMMON /RDTBL1/ TBLERR, NAMERR, INAMES, INLMX, NCOL, ICOL, NVAL,
     $                TBLPNT, TBLLIN
      COMMON /RDTBL2/ TBLNAM, TBLTYP, TBARAY
