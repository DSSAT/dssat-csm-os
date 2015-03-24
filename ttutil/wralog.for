      SUBROUTINE WRALOG (XNAME,X,ILDEC,IFND)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      INTEGER ILDEC,IFND
      CHARACTER*(*) XNAME
      LOGICAL X(ILDEC)

**    local variables ; dummy set
      INCLUDE 'wr_sys.inc'
      INTEGER XNAME_L, I1
      SAVE

      IF (.NOT.INIT) CALL FATALERR ('WRALOG','system not initialized')
      IF (IFND.GT.ILDEC) CALL FATALERR ('WRALOG','error')

      XNAME_L = LEN_TRIM (XNAME)

      IF (IFND.EQ.1) THEN
         IF (X(1)) THEN
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = 1*.TRUE.'
         ELSE
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = 1*.FALSE.'
         END IF
      ELSE
         IF (X(1)) THEN
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .TRUE.'
         ELSE
            WRITE (UNIT,'(1X,2A)') XNAME(1:XNAME_L),' = .FALSE.'
         END IF

         IF (IFND.GT.1) THEN
*           remaining data have to be written
            DO I1=2,IFND
               IF (X(I1)) THEN
                  WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .TRUE.'
               ELSE
                  WRITE (UNIT,'(1X,2A)')  SPACE(1:XNAME_L),'   .FALSE.'
               END IF
            END DO
         END IF
      END IF

      RETURN
      END
