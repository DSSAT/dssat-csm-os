*     index rerun file ; index available after CALL RDSETS
*     When you use the common blocks TTURR1 and TTURR2 the array lengths
*     in your declarations should correspond to the ones here !!!!!!
      INTEGER IARREP,IPTREP,INFREP
      CHARACTER REPLIS*31,REPTYP*1,REPARR*1
      DIMENSION REPLIS(ILNREP),REPTYP(ILNREP),REPARR(ILNREP)
      DIMENSION IARREP(ILPREP),IPTREP(ILPREP)

*     Active set number
      INTEGER ISCOM

      COMMON /TTURR1/ REPLIS,REPTYP,REPARR
      COMMON /TTURR2/ IARREP,IPTREP,INFREP,ISCOM

*     index data file ; index available after CALL RDINIT
*     When you use the common blocks TTUDD1 and TTUDD2 the array length
*     in your declarations should correspond to the one here !!!!!
      INTEGER IARDAT,IPTDAT,INFDAT
      CHARACTER DATLIS*31,DATTYP*1,DATARR*1
      DIMENSION DATLIS(ILNDAT),DATTYP(ILNDAT),DATARR(ILNDAT)
      DIMENSION IARDAT(ILNDAT),IPTDAT(ILNDAT)
      COMMON /TTUDD1/ DATLIS,DATTYP,DATARR
      COMMON /TTUDD2/ IARDAT,IPTDAT,INFDAT
