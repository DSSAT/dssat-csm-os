      SUBROUTINE WRATIM (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      CHARACTER*(*) XNAME
      INTEGER ILDEC,IFND
      DOUBLE PRECISION X(ILDEC)

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, STRING_L
      CHARACTER*80 STRING
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRATIM','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRATIM','error')

      DO I1=1,IFND
         CALL DTDPST ('YEAR-MONTHST-DAY_HOUR:MINUTE:SECONDS.FSECONDS',
     &                 X(I1),STRING)
         STRING_L = LEN_TRIM (STRING)
         IF (IFND.EQ.1) THEN
            XNAME_L = LEN_TRIM (XNAME)
            WRITE (UNIT,'(1X,3A)')
     &         XNAME(1:XNAME_L),' = 1*',STRING(1:STRING_L)
         ELSE IF (I1.EQ.1) THEN
            XNAME_L = LEN_TRIM (XNAME)
            WRITE (UNIT,'(1X,3A)')
     &         XNAME(1:XNAME_L),' = ',STRING(1:STRING_L)
         ELSE
            WRITE (UNIT,'(1X,3A)')
     &         SPACE(1:XNAME_L),'   ',STRING(1:STRING_L)
         END IF
      END DO

      RETURN
      END
