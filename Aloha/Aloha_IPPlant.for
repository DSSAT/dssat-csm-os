C=======================================================================
! Aloha_IPPlant, Subroutine
C  formerly IPIBS, Subroutine
C
C  Determines experiment and treatment selection
C-----------------------------------------------------------------------
C  Revision history
C
C  1. Written                                       G.H.        4-12-93
C  2  Modified by
C  3. Header revision and minor changes             P.W.W       6-28-93
C  4. Fixed units of DSOIL to be cm                 B.D.B.    19-APR-94
C  5. Change CN2 to CN                              B.D.B.     1-JUL-97
C-----------------------------------------------------------------------
C=======================================================================

	  SUBROUTINE Aloha_IPPlant ()

      IMPLICIT NONE

      CHARACTER*2  CROP
      CHARACTER*6  VARNO,ECONO
      CHARACTER*8  MODEL,EXPER
      CHARACTER*10 CROPS(35),SLNO,CROPD
      CHARACTER*16 VRNAME

      INTEGER IEMRG,TRTNO,YRSIM,YRPLT,ERRNUM,YEAR
      INTEGER LUNIO,PWDINF,PWDINL

      REAL    PLANTS,SDWTPL,SDAGE,ATEMP,PLPH,EFINOC,EFNFIX,AZIR
      REAL    FLOB,FLDD,SFDRN,SWPLTL,SWPLTH,SWPLTD,PTX,PTTN
      REAL    PLDS, PLME, ROWSPC, PLTPOP, SDEPTH, NFORCING, SPRLAP
      REAL    PLANTSIZE, G2, G3, P2, P3, P4, PHINT
      INTEGER PMTYPE, NDOF

C-----------------------------------------------------------------------
C    Read Cultivar Section
C-----------------------------------------------------------------------

      READ (LUNIO,56) CROP,VARNO,VRNAME

C-----------------------------------------------------------------------
C    Read Planting Details Section
C-----------------------------------------------------------------------

      READ (LUNIO,70) YRPLT,IEMRG,PLANTS,PLTPOP,PLME,PLDS,ROWSPC,AZIR,
     &                SDEPTH,SDWTPL,SDAGE,ATEMP,PLPH,SPRLAP,NFORCING,
     &                PLANTSIZE,NDOF,PMTYPE
C-PW  ROWSPC = ROWSPC / 100.0

C-----------------------------------------------------------------------
C     Read crop genetic information
C-----------------------------------------------------------------------

      IF (CROP .NE. 'FA') THEN
         READ (LUNIO,40)
         IF (CROP .EQ. 'PI') THEN
            READ (LUNIO,1800) VARNO,VRNAME,ECONO,P2,P3,P4,G2,G3,PHINT
         ENDIF
      ENDIF

      RETURN

C-----------------------------------------------------------------------
C     FORMAT Strings
C-----------------------------------------------------------------------
   40 FORMAT (A20)
   56 FORMAT (/,3X,A2,1X,A6,1X,A16)
   70 FORMAT (3X,I5,3X,I3,2(1X,F5.1),2(5X,A1),2(1X,F5.0),1X,F5.1,
     &        5(1X,F5.0),I6,F6.1,2(I6))
  100 FORMAT (3X,I5,4X,I2,2(1X,A5),2(1X,F5.0))
 1800 FORMAT (A6,1X,A16,1X,A6,1X,F6.1,F6.1,F6.0,F6.1,F6.2,F6.1)

      END Subroutine Aloha_IPPlant
