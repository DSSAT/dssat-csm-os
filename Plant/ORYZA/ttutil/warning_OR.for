      SUBROUTINE WARNING_OR (MODULE,MESSAG)
      IMPLICIT NONE

*     FORMAL_PARAMETERS
      CHARACTER*(*) MODULE, MESSAG

**    local variables
      INTEGER IL1,IL2
      LOGICAL TOSCR, TOLOG
      INTEGER UNLOG
!      CHARACTER*1 DUMMY
      SAVE

      IL1 = LEN_TRIM (MODULE)
      IL2 = LEN_TRIM (MESSAG)

*     desired output type
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

      IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
         IF (TOSCR) WRITE (*,'(A)')
     &   ' WARNING from unspecified source'
         IF (TOLOG) WRITE (UNLOG,'(A)')
     &   ' WARNING from unspecified source'

      ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
         IF (TOSCR) WRITE (*,'(2A)')
     &   ' WARNING from ',MODULE(1:IL1)
         IF (TOLOG) WRITE (UNLOG,'(2A)')
     &   ' WARNING from ',MODULE(1:IL1)

      ELSE
         IF (TOSCR) WRITE (*,'(4A)')
     &   ' WARNING from ',MODULE(1:IL1),': ',MESSAG(1:IL2)
         IF (TOLOG) WRITE (UNLOG,'(4A)')
     &   ' WARNING from ',MODULE(1:IL1),': ',MESSAG(1:IL2)
      END IF

      RETURN
      END
