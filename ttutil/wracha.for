      SUBROUTINE WRACHA (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME,X(ILDEC)

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1, I2
      INTEGER NCOL
      PARAMETER (NCOL=1)
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRACHA','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRACHA','error')

      XNAME_L = LEN_TRIM (XNAME)

      IF (IFND.EQ.1) THEN
         WRITE (UNIT,'(1X,5A)')
     &         XNAME(1:XNAME_L),' = 1*',Q,X(1),Q
      ELSE
         WRITE (UNIT,'(1X,2A,30A)')
     &         XNAME(1:XNAME_L),' = ',Q,X(1),Q,
     &         (',',Q,X(I1),Q,I1=2,MIN (NCOL, IFND))

         IF (IFND.GT.NCOL) THEN
*           remaining data have to be written
            DO I1=NCOL+1,IFND,NCOL
               WRITE (UNIT,'(1X,2A,30A)')
     &            SPACE(1:XNAME_L),'   ',Q,X(I1),Q,
     &            (',',Q,X(I2),Q,I2=I1+1,MIN (NCOL+I1-1, IFND))
            END DO
         END IF
      END IF

      RETURN
      END
