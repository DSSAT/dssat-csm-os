*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *  M A C H I N E   D E P E N D E N T   V A L U E S  *
*     *  ===============================================  *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *

*     IIL    - length in bytes of INTEGER datatype
*     IRL    - length in bytes of DOUBLE PRECISION datatype
*     ILL    - length in bytes of LOGICAL datatype
*     ILNREP - maximum number of names in each set of a rerun file
*     ILPREP - maximum number of assignments on a rerun file (40 sets
*              of 10 variables for instance, gives 400 assignments)
*     ILNDAT - maximum number of names in a data file
*              increase this number if you have more variables
*     INFDEC - maximum number of files in tmp file list

      INTEGER IIL, IRL, ILL, ILNREP, ILPREP, ILNDAT, INFDEC
      PARAMETER (IIL=4, IRL=8, ILL=4)
      PARAMETER (ILNREP=100,ILPREP=15000)
      PARAMETER (ILNDAT=400)
      PARAMETER (INFDEC=100)



*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *    B U F F E R   L E N G T H   I N   B Y T E S    *
*     *    ===========================================    *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *

*     ILBUF - length of TMP file buffers
*
      INTEGER ILBUF
      PARAMETER (ILBUF=128)
