      SUBROUTINE TTUVER (xMIN_V,xCUR_V)
      IMPLICIT NONE

*     FORMAL_PARAMETERS:
      REAL xMIN_V,xCUR_V

**    Local variables
      LOGICAL TOSCR, TOLOG
      INTEGER UNLOG
      REAL CUR_V
      PARAMETER (CUR_V=4.23)

*     desired output
      CALL MESSINQ (TOSCR, TOLOG, UNLOG)

      IF (xMIN_V.GT.CUR_V) THEN
         IF (TOSCR) WRITE (*,'(1X,A,/,1X,A,F5.2,/,1X,A,F5.2,A)')
     &     'This program is not linked with the minimal TTUTIL version',
     &     'This is TTUTIL version:',CUR_V,
     &     'At least version      :',xMIN_V,' is required'
         IF (TOLOG) WRITE (UNLOG,'(1X,A,/,1X,A,F5.2,/,1X,A,F5.2,A)')
     &     'This program is not linked with the minimal TTUTIL version',
     &     'This is TTUTIL version:',CUR_V,
     &     'At least version      :',xMIN_V,' is required'
         CALL FATALERR ('TTUVER',' ')
      END IF

      xCUR_V = CUR_V

      RETURN
      END
