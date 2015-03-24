      SUBROUTINE MESSWRT (MODULE,MESSAG)
      IMPLICIT NONE

*     Formal parameters
      CHARACTER*(*) MODULE, MESSAG

*     Local variables
      INTEGER IL1, IL2
      LOGICAL TOSCR, TOLOG
      INTEGER UNLOG

      SAVE

      IL1 = LEN_TRIM (MODULE)
      IL2 = LEN_TRIM (MESSAG)

*     desired output
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

      IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
         IF (TOSCR) WRITE (*,'(A)')
     &   ' WARNING from MESSWRT: both strings EMPTY'
         IF (TOLOG) WRITE (UNLOG,'(A)')
     &   ' WARNING from MESSWRT: both strings EMPTY'

      ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
         IF (TOSCR) WRITE (*,'(2A)')
     &   ' ',MODULE(1:IL1)
         IF (TOLOG) WRITE (UNLOG,'(2A)')
     &   ' ',MODULE(1:IL1)

      ELSE
         IF (TOSCR) WRITE (*,'(4A)')
     &   ' ',MODULE(1:IL1),': ',MESSAG(1:IL2)
         IF (TOLOG) WRITE (UNLOG,'(4A)')
     &   ' ',MODULE(1:IL1),': ',MESSAG(1:IL2)
      END IF

      RETURN
      END
